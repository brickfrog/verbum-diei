import romcal from "romcal";

type MaybeValue =
  | string
  | number
  | {
      value?: string | number;
      key?: string | number;
    };

type ObservancesMeta = {
  season: string;
  cycle: string;
  psalterWeek: string;
};

type ObservanceCelebration = {
  key: string;
  name: string;
  rank: string;
  source: string;
  color: string;
  titles: string[];
};

type Observances = {
  meta: ObservancesMeta;
  celebrations: ObservanceCelebration[];
};

type MomentLike = {
  format?: (format: string) => string;
};

type CalendarMeta = {
  cycle?: MaybeValue;
  psalterWeek?: MaybeValue;
  liturgicalColor?: { key?: MaybeValue; value?: MaybeValue };
  titles?: MaybeValue[];
};

type CalendarData = {
  meta?: CalendarMeta;
  season?: MaybeValue;
};

type CalendarEntry = {
  moment?: string | MomentLike;
  data?: CalendarData;
  key?: MaybeValue;
  name?: MaybeValue;
  type?: MaybeValue;
  source?: MaybeValue;
};

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null;
}

function safeString(value: unknown): string {
  if (value == null) return "";
  if (typeof value === "string") return value;
  if (typeof value === "number") return String(value);
  if (isRecord(value)) {
    if ("value" in value) return safeString(value.value);
    if ("key" in value) return safeString(value.key);
  }
  return String(value);
}

function dateKeyFromMoment(momentOrString: string | MomentLike | undefined): string {
  if (typeof momentOrString === "string") return momentOrString.slice(0, 10);
  if (momentOrString && typeof momentOrString.format === "function") {
    return momentOrString.format("YYYY-MM-DD");
  }
  return "";
}

export function getObservances(dateIso: string): () => Observances {
  return () => {
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
    ) as CalendarEntry[];

    const todays = calendar.filter(
      (entry) => dateKeyFromMoment(entry.moment) === dateIso,
    );

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
