import fs from "node:fs";

export function ensureDir(path) {
  return function () {
    fs.mkdirSync(path, { recursive: true });
  };
}

export function writeTextFile(path) {
  return function (contents) {
    return function () {
      fs.writeFileSync(path, contents, "utf8");
    };
  };
}

