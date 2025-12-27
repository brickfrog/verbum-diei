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
    ["malachi", "Malachias"],
    ["malachias", "Malachias"],
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

function getVerseText(data, book, chapter, verse) {
  const chapters = data.books?.[book];
  const chapterArr = Array.isArray(chapters) ? chapters[chapter - 1] : null;
  if (!Array.isArray(chapterArr)) return null;
  const t = chapterArr[verse - 1];
  return typeof t === "string" ? t : null;
}

const VERSE_MAPS = new Map([
  [
    "Malachias",
    {
      folds: [{ fromChapter: 3, toChapter: 4 }],
    },
  ],
  [
    "Zechariah",
    {
      offsets: [{ chapter: 2, offset: 4 }],
    },
  ],
]);

function mapVerseReference(data, book, chapter, verse) {
  const maps = VERSE_MAPS.get(book);
  if (!maps) return null;
  const chapters = data.books?.[book];
  if (!Array.isArray(chapters)) return null;

  for (const fold of maps.folds ?? []) {
    if (chapter !== fold.fromChapter) continue;
    const fromArr = chapters[fold.fromChapter - 1];
    const toArr = chapters[fold.toChapter - 1];
    if (!Array.isArray(fromArr) || !Array.isArray(toArr)) continue;
    const offset = fromArr.length;
    if (verse <= offset) continue;
    const mappedVerse = verse - offset;
    if (mappedVerse < 1 || mappedVerse > toArr.length) continue;
    return { chapter: fold.toChapter, verse: mappedVerse };
  }

  for (const mapping of maps.offsets ?? []) {
    if (chapter !== mapping.chapter) continue;
    const chapterArr = chapters[chapter - 1];
    if (!Array.isArray(chapterArr)) continue;
    const mappedVerse = verse - mapping.offset;
    if (mappedVerse < 1 || mappedVerse > chapterArr.length) continue;
    return { chapter, verse: mappedVerse };
  }

  return null;
}

export function fetchBibleReadingImpl(bookRaw) {
  return function (citation) {
    return function (refs) {
      return function (onError) {
        return function (onSuccess) {
          return function () {
            try {
              const data = loadData();
              const book = resolveBookName(bookRaw, data._bookByKey);
              if (!book) {
                throw new Error(`Unknown book: ${bookRaw}`);
              }

              if (!Array.isArray(refs) || refs.length === 0) {
                throw new Error(`Could not parse citation: ${citation}`);
              }

              const sameChapter = refs.every((r) => r.chapter === refs[0].chapter);
              const lineRefs = refs.map((r) =>
                sameChapter ? String(r.verse) : `${r.chapter}:${r.verse}`,
              );

              const lines = [];
              for (const r of refs) {
                let t = getVerseText(data, book, r.chapter, r.verse);
                if (!t) {
                  const mapped = mapVerseReference(data, book, r.chapter, r.verse);
                  if (mapped) t = getVerseText(data, book, mapped.chapter, mapped.verse);
                }
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
                lineRefs,
                lines,
              })();
            } catch (err) {
              onError(String(err))();
            }
          };
        };
      };
    };
  };
}
