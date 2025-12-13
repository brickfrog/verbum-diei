import OpenAI from "openai";

const VERBUM_SCHEMA = {
  type: "object",
  additionalProperties: false,
  properties: {
    marginalia: {
      type: "array",
      minItems: 6,
      maxItems: 18,
      items: {
        type: "object",
        additionalProperties: false,
        properties: {
          readingKind: { type: "string", enum: ["first", "gospel"] },
          lines: {
            type: "array",
            minItems: 1,
            maxItems: 4,
            items: { type: "integer", minimum: 1 },
          },
          text: { type: "string", minLength: 1, maxLength: 240 },
        },
        required: ["readingKind", "lines", "text"],
      },
    },
    commentary: {
      type: "object",
      additionalProperties: false,
      properties: {
        reading: {
          type: "array",
          minItems: 1,
          maxItems: 4,
          items: {
            type: "object",
            additionalProperties: false,
            properties: {
              lines: {
                type: "array",
                minItems: 1,
                maxItems: 6,
                items: { type: "integer", minimum: 1 },
              },
              text: { type: "string", minLength: 1, maxLength: 800 },
            },
            required: ["lines", "text"],
          },
        },
        gospel: {
          type: "array",
          minItems: 1,
          maxItems: 4,
          items: {
            type: "object",
            additionalProperties: false,
            properties: {
              lines: {
                type: "array",
                minItems: 1,
                maxItems: 6,
                items: { type: "integer", minimum: 1 },
              },
              text: { type: "string", minLength: 1, maxLength: 800 },
            },
            required: ["lines", "text"],
          },
        },
        synthesis: { type: "string", minLength: 1, maxLength: 800 },
      },
      required: ["reading", "gospel", "synthesis"],
    },
  },
  required: ["marginalia", "commentary"],
};

function normalizeLines(lines) {
  const numbers = Array.isArray(lines) ? lines : [];
  const uniq = new Set();
  for (const n of numbers) {
    if (Number.isFinite(n)) uniq.add(Math.trunc(n));
  }
  return Array.from(uniq).filter((n) => n >= 1).sort((a, b) => a - b);
}

function normalizeNote(note) {
  return {
    readingKind: note?.readingKind === "gospel" ? "gospel" : "first",
    lines: normalizeLines(note?.lines),
    text: typeof note?.text === "string" ? note.text.trim() : "",
  };
}

function normalizeCommentNote(note) {
  return {
    lines: normalizeLines(note?.lines),
    text: typeof note?.text === "string" ? note.text.trim() : "",
  };
}

function normalizeOutput(parsed) {
  const marginalia = Array.isArray(parsed?.marginalia)
    ? parsed.marginalia.map(normalizeNote).filter((n) => n.text && n.lines.length)
    : [];

  const reading = Array.isArray(parsed?.commentary?.reading)
    ? parsed.commentary.reading
        .map(normalizeCommentNote)
        .filter((n) => n.text && n.lines.length)
    : [];

  const gospel = Array.isArray(parsed?.commentary?.gospel)
    ? parsed.commentary.gospel
        .map(normalizeCommentNote)
        .filter((n) => n.text && n.lines.length)
    : [];

  const synthesis =
    typeof parsed?.commentary?.synthesis === "string"
      ? parsed.commentary.synthesis.trim()
      : "";

  return {
    marginalia,
    commentary: { reading, gospel, synthesis },
  };
}

function getResponseOutputText(response) {
  if (typeof response?.output_text === "string" && response.output_text) {
    return response.output_text;
  }

  const texts = [];
  for (const item of response?.output ?? []) {
    if (item?.type !== "message") continue;
    for (const content of item?.content ?? []) {
      if (content?.type === "output_text" && typeof content.text === "string") {
        texts.push(content.text);
      }
    }
  }
  return texts.join("");
}

function getResponseRefusal(response) {
  for (const item of response?.output ?? []) {
    if (item?.type !== "message") continue;
    for (const content of item?.content ?? []) {
      if (content?.type === "refusal" && typeof content.refusal === "string") {
        return content.refusal;
      }
    }
  }
  return "";
}

function summarizeResponse(response) {
  const outputTypes = [];
  const contentTypes = [];

  for (const item of response?.output ?? []) {
    if (item?.type) outputTypes.push(String(item.type));
    if (item?.type !== "message") continue;
    for (const content of item?.content ?? []) {
      if (content?.type) contentTypes.push(String(content.type));
    }
  }

  return { outputTypes, contentTypes };
}

export function callOpenAiStructuredImpl(model) {
  return function (instructions) {
    return function (input) {
      return function (temperature) {
        return function (onError) {
          return function (onSuccess) {
            return function () {
              const client = new OpenAI({
                apiKey: process.env.OPENAI_API_KEY,
              });

              void temperature;

              const request = {
                  model,
                  instructions,
                  input,
                  max_output_tokens: 1200,
                  text: {
                    verbosity: "low",
                    format: {
                      type: "json_schema",
                      name: "verbum_diei_analysis",
                      strict: true,
                      schema: VERBUM_SCHEMA,
                    },
                  },
                };

              client.responses
                .parse(request)
                .then((response) => {
                  const parsed = response?.output_parsed ?? null;
                  if (parsed) {
                    onSuccess(normalizeOutput(parsed))();
                    return;
                  }

                  const raw = getResponseOutputText(response);
                  const refusal = getResponseRefusal(response);
                  const summary = summarizeResponse(response);

                  if (raw) {
                    onSuccess(normalizeOutput(JSON.parse(raw)))();
                    return;
                  }

                  if (refusal) {
                    throw new Error(`Model refusal: ${refusal}`);
                  }

                  throw new Error(
                    `No parseable output (outputTypes=${JSON.stringify(
                      summary.outputTypes,
                    )}, contentTypes=${JSON.stringify(summary.contentTypes)})`,
                  );
                })
                .catch((err) => onError(String(err))());
            };
          };
        };
      };
    };
  };
}
