import fs from "node:fs";
import path from "node:path";

const DATA_PATH = path.join(process.cwd(), "assets", "bible", "dra1899.json");

let cached = null;

function loadData() {
  if (cached) return cached;
  const raw = fs.readFileSync(DATA_PATH, "utf8");
  const parsed = JSON.parse(raw);

  const byKey = new Map();
  for (const bookName of Object.keys(parsed?.books ?? {})) {
    byKey.set(normalizeBookKey(bookName), bookName);
  }

  // Extra aliases for common liturgical/alternate names.
  const aliasPairs = [
    ["ecclesiasticus", "Sirach"],
    ["sirach", "Sirach"],
    ["canticleofcanticles", "Song of Songs"],
    ["songofsongs", "Song of Songs"],
    ["apocalypse", "Revelation"],
    ["revelation", "Revelation"],
    ["tobias", "Tobit"],
    ["tobit", "Tobit"],
    ["josue", "Joshua"],
    ["joshua", "Joshua"],
    ["paralipomenon", "Chronicles"],
    ["machabees", "Maccabees"],
  ];

  for (const [fromKey, toBook] of aliasPairs) {
    const canonical = resolveBookName(toBook, byKey);
    if (canonical) byKey.set(fromKey, canonical);
  }

  cached = { ...parsed, _bookByKey: byKey };
  return cached;
}

function normalizeBookKey(name) {
  let s = String(name ?? "").trim();

  // Convert leading Roman numerals (I/II/III) to digits.
  const roman = s.match(/^(I{1,3})\s+(.*)$/i);
  if (roman) {
    const n = roman[1].length;
    s = `${n} ${roman[2]}`;
  }

  return s
    .toLowerCase()
    .replace(/\bsaint\b|\bst\.\b|\bst\b/gi, "")
    .replace(/[^a-z0-9]+/g, "");
}

function resolveBookName(input, byKey) {
  const key = normalizeBookKey(input);
  return byKey.get(key) ?? null;
}

function parseReference(reference) {
  const trimmed = String(reference ?? "").trim();
  const match = trimmed.match(/^(.+)\s+(\d+.*)$/);
  if (!match) {
    throw new Error(`Could not parse reference: ${trimmed}`);
  }
  return { bookRaw: match[1].trim(), citation: match[2].trim() };
}

function parseCitation(citation) {
  const cleaned = String(citation ?? "").replace(/\s+/g, "");
  const segments = cleaned.split(";").filter(Boolean);
  const out = [];

  for (const seg of segments) {
    const m = seg.match(/^(\d+):(.+)$/);
    if (!m) continue;

    const chapter = Number.parseInt(m[1], 10);
    const versesPart = m[2];

    for (const part of versesPart.split(",").filter(Boolean)) {
      const range = part.match(/^(\d+)-(\d+)$/);
      if (range) {
        const start = Number.parseInt(range[1], 10);
        const end = Number.parseInt(range[2], 10);
        const lo = Math.min(start, end);
        const hi = Math.max(start, end);
        for (let v = lo; v <= hi; v++) out.push({ chapter, verse: v });
        continue;
      }

      if (/^\d+$/.test(part)) {
        out.push({ chapter, verse: Number.parseInt(part, 10) });
      }
    }
  }

  return out;
}

function getVerseText(data, book, chapter, verse) {
  const chapters = data.books?.[book];
  const chapterArr = Array.isArray(chapters) ? chapters[chapter - 1] : null;
  if (!Array.isArray(chapterArr)) return null;
  const t = chapterArr[verse - 1];
  return typeof t === "string" ? t : null;
}

export function fetchBibleReadingImpl(reference) {
  return function (onError) {
    return function (onSuccess) {
      return function () {
        try {
          const data = loadData();
          const { bookRaw, citation } = parseReference(reference);
          const book = resolveBookName(bookRaw, data._bookByKey);
          if (!book) {
            throw new Error(`Unknown book: ${bookRaw}`);
          }

          const refs = parseCitation(citation);
          if (!refs.length) {
            throw new Error(`Could not parse citation: ${citation}`);
          }

          const lines = [];
          for (const r of refs) {
            const t = getVerseText(data, book, r.chapter, r.verse);
            if (!t) {
              throw new Error(
                `Missing verse text for ${book} ${r.chapter}:${r.verse}`,
              );
            }
            lines.push(t);
          }

          const translation = {
            id: data.translation?.id ?? "dra",
            name: data.translation?.name ?? "Douay-Rheims 1899 American Edition",
            note: data.translation?.note ?? "Public Domain",
          };

          onSuccess({
            reference: `${book} ${citation}`,
            translation,
            lines,
          })();
        } catch (err) {
          onError(String(err))();
        }
      };
    };
  };
}

