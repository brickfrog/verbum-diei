const BASE_URL = "https://bible-api.com/";

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
        const url = new URL(`${BASE_URL}${encodeURIComponent(bibleApiReference)}`);
        url.searchParams.set("translation", "web");

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
              id: json?.translation_id ?? "web",
              name: json?.translation_name ?? "World English Bible",
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

