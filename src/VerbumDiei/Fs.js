import fs from "node:fs";

export function ensureDir(path) {
  return function () {
    fs.mkdirSync(path, { recursive: true });
  };
}

export function readDir(path) {
  return function () {
    try {
      return fs.readdirSync(path);
    } catch (_err) {
      return [];
    }
  };
}

export function writeTextFile(path) {
  return function (contents) {
    return function () {
      fs.writeFileSync(path, contents, "utf8");
    };
  };
}
