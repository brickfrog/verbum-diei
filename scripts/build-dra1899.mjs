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

function parseGutenbergText(text) {
  const books = {};

  let currentBook = null;
  let currentChapter = null;
  let currentVerse = null;
  let verseParts = [];

  function flushVerse() {
    if (!currentBook || !currentChapter || !currentVerse) return;

    const chapterArr = ensureChapter(books, currentBook, currentChapter);
    const joined = verseParts.join(" ").replace(/\s+/g, " ").trim();
    chapterArr[currentVerse - 1] = joined;

    currentVerse = null;
    verseParts = [];
  }

  const lines = text.replace(/\r\n/g, "\n").split("\n");
  for (const rawLine of lines) {
    const line = rawLine.replace(/^\uFEFF/, "").trimEnd();

    const chapterMatch = line.match(/^(.+?) Chapter (\d+)\s*$/);
    if (chapterMatch) {
      flushVerse();
      currentBook = canonicalBookName(chapterMatch[1].trim());
      currentChapter = Number.parseInt(chapterMatch[2], 10);
      ensureChapter(books, currentBook, currentChapter);
      continue;
    }

    const verseMatch = line.match(/^(\d+):(\d+)\.\s*(.*)$/);
    if (verseMatch && currentBook && currentChapter) {
      flushVerse();
      currentVerse = Number.parseInt(verseMatch[2], 10);
      verseParts = [String(verseMatch[3] ?? "").trim()].filter(Boolean);
      continue;
    }

    if (currentVerse != null) {
      const t = line.trim();
      if (t) verseParts.push(t);
    }
  }

  flushVerse();
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

