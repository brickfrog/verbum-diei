import romcal from "romcal";

function safeString(value) {
  if (value == null) return "";
  if (typeof value === "string") return value;
  if (typeof value === "number") return String(value);
  if (typeof value === "object" && "value" in value) return safeString(value.value);
  if (typeof value === "object" && "key" in value) return safeString(value.key);
  return String(value);
}

function dateKeyFromMoment(momentOrString) {
  if (typeof momentOrString === "string") return momentOrString.slice(0, 10);
  if (momentOrString && typeof momentOrString.format === "function") {
    return momentOrString.format("YYYY-MM-DD");
  }
  return "";
}

export function getObservances(dateIso) {
  return function () {
    const [yearStr] = dateIso.split("-");
    const year = Number(yearStr);
    if (!Number.isFinite(year)) {
      return {
        meta: { season: "", cycle: "", psalterWeek: "" },
        celebrations: [],
      };
    }

    const calendar = romcal.calendarFor(
      { year, country: "unitedStates", locale: "en", type: "calendar" },
      false,
    );

    const todays = calendar.filter((c) => dateKeyFromMoment(c.moment) === dateIso);

    const primary = todays[0];
    const meta = primary?.data?.meta ?? {};
    const season = safeString(primary?.data?.season);
    const cycle = safeString(meta?.cycle);
    const psalterWeek = safeString(meta?.psalterWeek);

    const celebrations = todays.map((c) => ({
      key: safeString(c.key),
      name: safeString(c.name),
      rank: safeString(c.type),
      source: safeString(c.source),
      color: safeString(c.data?.meta?.liturgicalColor?.key),
      titles: Array.isArray(c.data?.meta?.titles)
        ? c.data.meta.titles.map(safeString)
        : [],
    }));

    return {
      meta: { season, cycle, psalterWeek },
      celebrations,
    };
  };
}

