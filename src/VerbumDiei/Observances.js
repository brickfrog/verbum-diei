import romcal from "romcal";
function isRecord(value) {
    return typeof value === "object" && value !== null;
}
function safeString(value) {
    if (value == null)
        return "";
    if (typeof value === "string")
        return value;
    if (typeof value === "number")
        return String(value);
    if (isRecord(value)) {
        if ("value" in value)
            return safeString(value.value);
        if ("key" in value)
            return safeString(value.key);
    }
    return String(value);
}
function dateKeyFromMoment(momentOrString) {
    if (typeof momentOrString === "string")
        return momentOrString.slice(0, 10);
    if (momentOrString && typeof momentOrString.format === "function") {
        return momentOrString.format("YYYY-MM-DD");
    }
    return "";
}
export function getObservances(dateIso) {
    return () => {
        const [yearStr] = dateIso.split("-");
        const year = Number(yearStr);
        if (!Number.isFinite(year)) {
            return {
                meta: { season: "", cycle: "", psalterWeek: "" },
                celebrations: [],
            };
        }
        const calendar = romcal.calendarFor({ year, country: "unitedStates", locale: "en", type: "calendar" }, false);
        const todays = calendar.filter((entry) => dateKeyFromMoment(entry.moment) === dateIso);
        const primary = todays[0];
        const meta = primary?.data?.meta;
        const season = safeString(primary?.data?.season);
        const cycle = safeString(meta?.cycle);
        const psalterWeek = safeString(meta?.psalterWeek);
        const celebrations = todays.map((entry) => ({
            key: safeString(entry.key),
            name: safeString(entry.name),
            rank: safeString(entry.type),
            source: safeString(entry.source),
            color: safeString(entry.data?.meta?.liturgicalColor?.key),
            titles: Array.isArray(entry.data?.meta?.titles)
                ? entry.data.meta.titles.map(safeString)
                : [],
        }));
        return {
            meta: { season, cycle, psalterWeek },
            celebrations,
        };
    };
}
