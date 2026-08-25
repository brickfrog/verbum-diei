import fs from "node:fs";
import path from "node:path";

const SOURCE_URL = "https://www.gutenberg.org/cache/epub/8300/pg8300.txt";
const SOURCE_TXT = path.join(process.cwd(), "scripts", "dra1899.source.txt");
const OUT_JSON = path.join(process.cwd(), "assets", "bible", "dra1899.json");

const BOOK_MAP = {
  Josue: "Joshua",
  "1 Kings": "1 Samuel",
  "2 Kings": "2 Samuel",
  "3 Kings": "1 Kings",
  "4 Kings": "2 Kings",
  "1 Paralipomenon": "1 Chronicles",
  "2 Paralipomenon": "2 Chronicles",
  "1 Esdras": "Ezra",
  "2 Esdras": "Nehemiah",
  Tobias: "Tobit",
  "Canticle of Canticles": "Song of Songs",
  Ecclesiasticus: "Sirach",
  Isaias: "Isaiah",
  Jeremias: "Jeremiah",
  Ezechiel: "Ezekiel",
  Osee: "Hosea",
  Abdias: "Obadiah",
  Jonas: "Jonah",
  Micheas: "Micah",
  Habacuc: "Habakkuk",
  Sophonias: "Zephaniah",
  Aggeus: "Haggai",
  Zacharias: "Zechariah",
  "1 Machabees": "1 Maccabees",
  "2 Machabees": "2 Maccabees",
  Apocalypse: "Revelation",
};

function canonicalBookName(raw) {
  return BOOK_MAP[raw] ?? raw;
}

function ensureChapter(books, book, chapterNumber) {
  const bookChapters = (books[book] ??= []);
  while (bookChapters.length < chapterNumber) bookChapters.push([]);
  return bookChapters[chapterNumber - 1];
}

// The source separates every block with a blank line. A block is scripture only
// when it opens with a "chapter:verse" marker; everything else is a chapter
// summary, a book preface, a Challoner footnote, or Gutenberg boilerplate, and
// must be dropped rather than folded into the preceding verse.
//
// The marker separator varies ("1:1.", "1:18:", "12:20"), markers occasionally
// appear mid-block, and chapter headings sometimes carry a trailing period or
// run into their summary. Chapter and verse numbers are therefore taken from
// the marker itself; the heading only establishes which book we are in.
const VERSE_MARKER = /^(\d+):(\d+)(?:[.:][ \t]|[ \t])\s*([\s\S]*)$/;
const INLINE_MARKER = /(\d+):(\d+)[.:]\s+/g;
const CHAPTER_HEADING = /^(.+?) Chapter (\d+)(?:[.\s]|$)/;

// The source prints Vulgate psalm 9 in two halves, restarting the verse
// numbering at 1 after this divider. The second half is psalm 9:22-39.
const HEBREW_PSALM_DIVIDER = "(Psalm Chapter 10 according to the Hebrews.)";

// Recovers verses run together inside one block, e.g. "4:31. ...  4:32. ...".
// A split is accepted only when the marker names the current chapter and the
// next verse in sequence, so numerals inside scripture cannot trigger one.
function splitInlineVerses(chapter, firstVerse, text) {
  const parts = [];
  let verse = firstVerse;
  let start = 0;
  for (const match of text.matchAll(INLINE_MARKER)) {
    if (Number.parseInt(match[1], 10) !== chapter) continue;
    if (Number.parseInt(match[2], 10) !== verse + 1) continue;
    parts.push({ verse, text: text.slice(start, match.index).trim() });
    verse += 1;
    start = match.index + match[0].length;
  }
  parts.push({ verse, text: text.slice(start).trim() });
  return parts;
}

function parseGutenbergText(text) {
  const books = {};

  let currentBook = null;
  let verseOffset = 0;
  let lastVerse = 0;

  const normalised = text.replace(/^\uFEFF/, "").replace(/\r\n/g, "\n");

  for (const rawBlock of normalised.split(/\n\s*\n/)) {
    const block = rawBlock.trim();
    if (!block) continue;

    if (block.replace(/\s+/g, " ") === HEBREW_PSALM_DIVIDER) {
      verseOffset = lastVerse;
      continue;
    }

    if (!/^\d+:\d+/.test(block)) {
      const heading = block.split("\n")[0].trim().match(CHAPTER_HEADING);
      if (heading) {
        currentBook = canonicalBookName(heading[1].trim());
        ensureChapter(books, currentBook, Number.parseInt(heading[2], 10));
        verseOffset = 0;
        lastVerse = 0;
      }
      continue;
    }

    const marker = block.match(VERSE_MARKER);
    if (!marker || !currentBook) continue;

    const chapter = Number.parseInt(marker[1], 10);
    const chapterArr = ensureChapter(books, currentBook, chapter);
    const body = marker[3].replace(/\s+/g, " ").trim();

    for (const part of splitInlineVerses(chapter, Number.parseInt(marker[2], 10), body)) {
      const verse = part.verse + verseOffset;
      chapterArr[verse - 1] = part.text;
      lastVerse = verse;
    }
  }

  return books;
}

async function main() {
  if (!fs.existsSync(SOURCE_TXT)) {
    console.error(
      `Missing ${SOURCE_TXT}.\n\n` +
        `Download the source text first (or fetch it yourself):\n` +
        `  curl -fsSL -o ${SOURCE_TXT} ${SOURCE_URL}\n`,
    );
    process.exit(1);
  }

  const txt = fs.readFileSync(SOURCE_TXT, "utf8");
  const books = parseGutenbergText(txt);

  const out = {
    translation: {
      id: "dra",
      name: "Douay-Rheims 1899 American Edition",
      note: "Public Domain",
      source: "Project Gutenberg eBook #8300 (Challoner revision)",
    },
    books,
  };

  fs.mkdirSync(path.dirname(OUT_JSON), { recursive: true });
  fs.writeFileSync(OUT_JSON, JSON.stringify(out), "utf8");
  console.log(`Wrote ${OUT_JSON}`);
}

await main();

