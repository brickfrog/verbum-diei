import crypto from "node:crypto";

export function nowIso() {
  return function () {
    return new Date().toISOString();
  };
}

export function sha256Hex(input) {
  return function () {
    return crypto.createHash("sha256").update(input, "utf8").digest("hex");
  };
}

