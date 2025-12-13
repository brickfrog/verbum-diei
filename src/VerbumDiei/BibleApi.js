const BASE_URL = "https://bible-api.com/";

function preferredTranslationId() {
  const raw = process.env.VERBUM_BIBLE_TRANSLATION;
  const t = typeof raw === "string" ? raw.trim().toLowerCase() : "";
  return t || "dra";
}

function splitLines(text) {
  return text
    .replace(/\r\n/g, "\n")
    .split("\n")
    .map((l) => l.trim())
    .filter(Boolean);
}

export function fetchBibleApiReadingImpl(bibleApiReference) {
  return function (onError) {
    return function (onSuccess) {
      return function () {
        const translationId = preferredTranslationId();
        const url = new URL(`${BASE_URL}${encodeURIComponent(bibleApiReference)}`);
        url.searchParams.set("translation", translationId);

        fetch(url.toString())
          .then((res) => {
            if (!res.ok) {
              throw new Error(`Bible API HTTP ${res.status} ${res.statusText}`);
            }
            return res.json();
          })
          .then((json) => {
            const reference = json?.reference ?? bibleApiReference;
            const translation = {
              id: json?.translation_id ?? translationId,
              name: json?.translation_name ?? "Douay-Rheims 1899 American Edition",
              note: json?.translation_note ?? "Public Domain",
            };
            const lines = splitLines(json?.text ?? "");

            onSuccess({ reference, translation, lines })();
          })
          .catch((err) => onError(String(err))());
      };
    };
  };
}
