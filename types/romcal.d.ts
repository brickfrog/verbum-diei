declare module "romcal" {
  export type RomcalCalendarType = "calendar" | "liturgical";

  export interface RomcalCalendarConfig {
    year: number;
    country: string;
    locale: string;
    type: RomcalCalendarType;
    query?: Record<string, unknown>;
  }

  export interface RomcalValue {
    value?: string | number;
    key?: string | number;
  }

  export interface RomcalMeta {
    cycle?: string | number | RomcalValue;
    psalterWeek?: string | number | RomcalValue;
    liturgicalColor?: RomcalValue;
    titles?: Array<string | number | RomcalValue>;
  }

  export interface RomcalData {
    meta?: RomcalMeta;
    season?: string | number | RomcalValue;
  }

  export interface RomcalMomentLike {
    format?: (format: string) => string;
  }

  export interface RomcalCalendarEntry {
    moment?: string | RomcalMomentLike;
    data?: RomcalData;
    key?: string | number | RomcalValue;
    name?: string | number | RomcalValue;
    type?: string | number | RomcalValue;
    source?: string | number | RomcalValue;
  }

  const romcal: {
    calendarFor: (
      config: RomcalCalendarConfig,
      skipIsoConversion?: boolean,
    ) => RomcalCalendarEntry[];
  };

  export default romcal;
}
