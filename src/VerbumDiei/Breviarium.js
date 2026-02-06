import { Breviarium } from "breviarium";

function safeString(value) {
  if (value == null) return "";
  if (typeof value === "string") return value;
  if (typeof value === "number") return String(value);
  return String(value);
}

function parseIsoDate(dateIso) {
  const m = String(dateIso || "").match(/^(\d{4})-(\d{2})-(\d{2})$/);
  if (!m) return null;
  const year = Number(m[1]);
  const month = Number(m[2]);
  const day = Number(m[3]);
  if (!Number.isFinite(year) || !Number.isFinite(month) || !Number.isFinite(day)) {
    return null;
  }
  return new Date(year, month - 1, day, 12, 0, 0, 0);
}

function firstNonEmpty(values) {
  for (const value of values) {
    const text = safeString(value).trim();
    if (text !== "") {
      return text;
    }
  }
  return "";
}

function normalizeOfficeOption(item) {
  const entry = item || {};
  return {
    id: safeString(entry.id),
    cycle: safeString(entry.cycle),
    readingRef: firstNonEmpty([
      entry.lectura_biblica_cita,
      entry.lectura_biblica_cita_a,
      entry.lectura_biblica_cita_i,
      entry.lectura_biblica_cita_p,
    ]),
    reading: firstNonEmpty([
      entry.lectura_biblica,
      entry.lectura_biblica_texto,
      entry.lectura_biblica_texto_a,
      entry.lectura_biblica_texto_i,
      entry.lectura_biblica_texto_p,
    ]),
    finalPrayer: firstNonEmpty([entry.oracion_final, entry.final, entry.oracion]),
  };
}

function normalizeOfficeList(value) {
  if (Array.isArray(value)) {
    return value.map(normalizeOfficeOption);
  }
  if (value && typeof value === "object") {
    return [normalizeOfficeOption(value)];
  }
  return [];
}

export function getOfficePayloadPromise(dateIso) {
  return () => {
    const date = parseIsoDate(dateIso);
    if (!date) {
      return Promise.resolve({
        officium: [],
        laudes: [],
        tertia: [],
        sexta: [],
        nona: [],
        vesperae: [],
        completorium: [],
      });
    }

    const breviarium = new Breviarium(date);
    return Promise.all([
      breviarium.getOfficium(date),
      breviarium.getLaudes(date),
      breviarium.getTertia(date),
      breviarium.getSexta(date),
      breviarium.getNona(date),
      breviarium.getVesperae(date),
      breviarium.getCompletorium(date),
    ])
      .then((results) => {
        const [officium, laudes, tertia, sexta, nona, vesperae, completorium] = results;
        return {
          officium: normalizeOfficeList(officium),
          laudes: normalizeOfficeList(laudes),
          tertia: normalizeOfficeList(tertia),
          sexta: normalizeOfficeList(sexta),
          nona: normalizeOfficeList(nona),
          vesperae: normalizeOfficeList(vesperae),
          completorium: normalizeOfficeList(completorium),
        };
      })
      .catch(() => ({
        officium: [],
        laudes: [],
        tertia: [],
        sexta: [],
        nona: [],
        vesperae: [],
        completorium: [],
      }));
  };
}
