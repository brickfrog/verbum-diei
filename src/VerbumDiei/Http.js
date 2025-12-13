export function fetchTextImpl(url) {
  return function (onError) {
    return function (onSuccess) {
      return function () {
        fetch(url)
          .then((res) => {
            if (!res.ok) {
              throw new Error(`HTTP ${res.status} ${res.statusText}`);
            }
            return res.text();
          })
          .then((text) => onSuccess(text)())
          .catch((err) => onError(String(err))());
      };
    };
  };
}

