import { XMLParser } from "fast-xml-parser";

function decodeEntities(input) {
  return input
    .replace(/&nbsp;/g, " ")
    .replace(/&quot;/g, '"')
    .replace(/&amp;/g, "&")
    .replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .replace(/&#39;/g, "'")
    .replace(/&apos;/g, "'")
    .replace(/&#x([0-9a-fA-F]+);/g, (_, hex) =>
      String.fromCharCode(Number.parseInt(hex, 16)),
    )
    .replace(/&#([0-9]+);/g, (_, num) => String.fromCharCode(Number(num)));
}

function stripTags(html) {
  return decodeEntities(
    html
      .replace(/<br\s*\/?>/gi, "\n")
      .replace(/<\/?[^>]+>/g, "")
      .replace(/\r\n/g, "\n"),
  );
}

function extractParagraphs(html) {
  const matches = html.match(/<p\b[^>]*>[\s\S]*?<\/p>/gi);
  return (matches ?? []).map((p) => stripTags(p).trim());
}

function bookFromHeading(heading) {
  const trimmed = (heading ?? "").trim();

  function clean(name) {
    return String(name ?? "")
      .replace(/^the\s+prophet\s+/i, "")
      .replace(/^(saint|st\.?)\s+/i, "")
      .trim();
  }

  if (/acts of the apostles/i.test(trimmed)) return "Acts";

  const paulMatch = trimmed.match(
    /Letter of\s+(?:Saint|St\.?)\s+Paul\s+to\s+the\s+(.+)$/i,
  );
  if (paulMatch) return clean(paulMatch[1]);

  const letterMatch = trimmed.match(/Letter of\s+(?:Saint|St\.?)\s+(.+)$/i);
  if (letterMatch) return clean(letterMatch[1]);

  const bookMatch = heading.match(/Book of\s+(.+)$/i);
  if (bookMatch) return clean(bookMatch[1]);

  const gospelMatch = heading.match(/Gospel according to\s+(.+)$/i);
  if (gospelMatch) return clean(gospelMatch[1]);

  return null;
}

function normalizeCitation(citation) {
  return citation
    .replace(/\s+/g, " ")
    .replace(/,\s+/g, ",")
    .replace(/(\d+)[a-z]/gi, "$1")
    .trim();
}

function extractReadingsFromDescription(descriptionHtml) {
  const paragraphs = extractParagraphs(descriptionHtml);

  const firstReadingHeading = paragraphs.find((p) =>
    /^A reading from/i.test(p),
  );
  const gospelHeading = paragraphs.find((p) =>
    /Gospel according to/i.test(p),
  );

  const results = [];

  if (firstReadingHeading) {
    const lines = firstReadingHeading
      .split("\n")
      .map((l) => l.trim())
      .filter(Boolean);
    const heading = lines[0] ?? "A reading";
    const citation = normalizeCitation(lines[1] ?? "");
    const book = bookFromHeading(heading) ?? "";
    const bibleApiReference = `${book} ${citation}`.trim();

    results.push({
      kind: "first",
      heading,
      book,
      citation,
      bibleApiReference,
    });
  }

  if (gospelHeading) {
    const lines = gospelHeading
      .split("\n")
      .map((l) => l.trim())
      .filter(Boolean);
    const heading = lines[0] ?? "Gospel";
    const citation = normalizeCitation(lines[1] ?? "");
    const book = bookFromHeading(heading) ?? "";
    const bibleApiReference = `${book} ${citation}`.trim();

    results.push({
      kind: "gospel",
      heading,
      book,
      citation,
      bibleApiReference,
    });
  }

  return results;
}

function dateFromGuid(guid) {
  const match = guid.match(/\/(\d{4})\/(\d{2})\/(\d{2})\.html$/);
  if (!match) return "";
  return `${match[1]}-${match[2]}-${match[3]}`;
}

export function parseWordOfDayFeed(xml) {
  const parser = new XMLParser({
    ignoreAttributes: false,
    attributeNamePrefix: "",
    trimValues: true,
    parseTagValue: false,
  });

  const parsed = parser.parse(xml);
  const channel = parsed?.rss?.channel ?? {};

  const rawItems = channel.item
    ? Array.isArray(channel.item)
      ? channel.item
      : [channel.item]
    : [];

  const items = rawItems.map((item) => {
    const descriptionHtml = item.description ?? "";
    const guid = item.guid ?? "";
    return {
      title: item.title ?? "",
      guid,
      date: dateFromGuid(guid),
      pubDate: item.pubDate ?? "",
      descriptionHtml,
      readings: extractReadingsFromDescription(descriptionHtml),
    };
  });

  return {
    title: channel.title ?? "",
    link: channel.link ?? "",
    items,
  };
}
