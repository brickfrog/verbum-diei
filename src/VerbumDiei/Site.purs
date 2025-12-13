module VerbumDiei.Site
  ( renderArtifactPage
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String as String
import VerbumDiei.Artifact (Artifact, CommentNote, MarginalNote, Reading, ReadingKind, firstReadingKind, gospelKind)

escapeHtml :: String -> String
escapeHtml =
  replaceAll (Pattern "&") (Replacement "&amp;")
    >>> replaceAll (Pattern "<") (Replacement "&lt;")
    >>> replaceAll (Pattern ">") (Replacement "&gt;")
    >>> replaceAll (Pattern "\"") (Replacement "&quot;")
    >>> replaceAll (Pattern "'") (Replacement "&#39;")

kindLabel :: ReadingKind -> String
kindLabel = case _ of
  k | k == gospelKind -> "Gospel"
  _ -> "Reading"

kindShort :: ReadingKind -> String
kindShort = case _ of
  k | k == gospelKind -> "G"
  _ -> "R"

lineId :: ReadingKind -> Int -> String
lineId kind n =
  kind <> "-L" <> show n

renderLine :: ReadingKind -> Int -> String -> String
renderLine kind n text =
  "<p class=\"scripture-line\" id=\"" <> escapeHtml (lineId kind n) <> "\">"
    <> "<span class=\"ln\">" <> show n <> "</span>"
    <> "<span class=\"lt\">" <> escapeHtml text <> "</span>"
    <> "</p>"

renderReadingBox :: Reading -> String
renderReadingBox reading =
  let
    linesHtml =
      String.joinWith "" $
        reading.lines # Array.mapWithIndex \i line ->
          renderLine reading.kind (i + 1) line
  in
    "<section class=\"box reading-box\">"
      <> "<header class=\"section-header\">"
      <> "<div class=\"section-kicker\">" <> escapeHtml (kindLabel reading.kind) <> "</div>"
      <> "<h2 class=\"section-title\">" <> escapeHtml reading.heading <> "</h2>"
      <> "<div class=\"section-ref\">" <> escapeHtml reading.reference <> "</div>"
      <> "<div class=\"section-meta\">"
      <> escapeHtml reading.translation.name <> " — " <> escapeHtml reading.translation.note
      <> "</div>"
      <> "</header>"
      <> "<article class=\"scripture\">"
      <> linesHtml
      <> "</article>"
      <> "</section>"

renderObservances :: Artifact -> String
renderObservances artifact =
  let
    meta =
      if artifact.observances.meta.season == "" && artifact.observances.meta.cycle == "" && artifact.observances.meta.psalterWeek == "" then ""
      else
        "<div class=\"observances-meta\">"
          <> "<div><span class=\"label\">Season</span> " <> escapeHtml artifact.observances.meta.season <> "</div>"
          <> "<div><span class=\"label\">Cycle</span> " <> escapeHtml artifact.observances.meta.cycle <> "</div>"
          <> "<div><span class=\"label\">Psalter</span> " <> escapeHtml artifact.observances.meta.psalterWeek <> "</div>"
          <> "</div>"

    celebrations =
      String.joinWith "" $
        artifact.observances.celebrations <#> \c ->
          "<li class=\"celebration\">"
            <> "<span class=\"celebration-rank\">" <> escapeHtml c.rank <> "</span>"
            <> "<span class=\"celebration-name\">" <> escapeHtml c.name <> "</span>"
            <> "</li>"
  in
    "<section class=\"box observances-box\">"
      <> "<header class=\"section-header\">"
      <> "<div class=\"section-kicker\">Day</div>"
      <> "<h2 class=\"section-title\">Observances</h2>"
      <> "</header>"
      <> meta
      <> "<ul class=\"celebrations\">"
      <> celebrations
      <> "</ul>"
      <> "</section>"

renderLinesLabel :: ReadingKind -> Array Int -> String
renderLinesLabel kind lines =
  kindShort kind <> " " <> String.joinWith "," (lines <#> show)

renderMarginalia :: Boolean -> Array MarginalNote -> String
renderMarginalia hasLlm notes =
  if Array.length notes == 0 then
    if hasLlm then "<div class=\"empty\">(no marginalia generated)</div>"
    else "<div class=\"empty\">LLM output unavailable. Set <code>OPENAI_API_KEY</code> and re-run <code>bun run generate</code>.</div>"
  else
    "<ol class=\"notes\">"
      <> String.joinWith "" (notes <#> renderNote)
      <> "</ol>"
  where
  renderNote :: MarginalNote -> String
  renderNote note =
    let
      firstLine = Array.head note.lines
      href = case firstLine of
        Nothing -> "#"
        Just n -> "#" <> lineId note.readingKind n
      label = renderLinesLabel note.readingKind note.lines
    in
      "<li class=\"note\">"
        <> "<a class=\"note-ref\" href=\"" <> escapeHtml href <> "\">"
        <> escapeHtml label
        <> "</a>"
        <> "<span class=\"note-text\">" <> escapeHtml note.text <> "</span>"
        <> "</li>"

renderCommentNotes :: ReadingKind -> Array CommentNote -> String
renderCommentNotes kind notes =
  if Array.length notes == 0 then ""
  else
    "<ol class=\"comment-notes\">"
      <> String.joinWith "" (notes <#> renderOne)
      <> "</ol>"
  where
  renderOne :: CommentNote -> String
  renderOne note =
    let
      firstLine = Array.head note.lines
      href = case firstLine of
        Nothing -> "#"
        Just n -> "#" <> lineId kind n
      label = renderLinesLabel kind note.lines
    in
      "<li class=\"comment-note\">"
        <> "<a class=\"note-ref\" href=\"" <> escapeHtml href <> "\">"
        <> escapeHtml label
        <> "</a>"
        <> "<span class=\"note-text\">" <> escapeHtml note.text <> "</span>"
        <> "</li>"

renderCommentaryBox :: Boolean -> Artifact -> String
renderCommentaryBox hasLlm artifact =
  let
    isEmpty =
      Array.length artifact.commentary.reading == 0
        && Array.length artifact.commentary.gospel == 0
        && artifact.commentary.synthesis == ""

    emptyMessage =
      if hasLlm then "<div class=\"empty\">(no commentary generated)</div>"
      else "<div class=\"empty\">LLM output unavailable. Set <code>OPENAI_API_KEY</code> and re-run <code>bun run generate</code>.</div>"
  in
    "<section class=\"box commentary-box\">"
      <> "<header class=\"section-header\">"
      <> "<div class=\"section-kicker\">Gloss</div>"
      <> "<h2 class=\"section-title\">Commentary</h2>"
      <> "</header>"
      <> (if isEmpty then emptyMessage else
          "<div class=\"commentary-sections\">"
            <> (if Array.length artifact.commentary.reading == 0 then "" else
                "<div class=\"commentary-section\">"
                  <> "<div class=\"section-kicker\">On the Reading</div>"
                  <> renderCommentNotes firstReadingKind artifact.commentary.reading
                  <> "</div>")
            <> (if Array.length artifact.commentary.gospel == 0 then "" else
                "<div class=\"commentary-section\">"
                  <> "<div class=\"section-kicker\">On the Gospel</div>"
                  <> renderCommentNotes gospelKind artifact.commentary.gospel
                  <> "</div>")
            <> "</div>"
            <> (if artifact.commentary.synthesis == "" then "" else
                "<p class=\"commentary-synthesis\">" <> escapeHtml artifact.commentary.synthesis <> "</p>"))
      <> "</section>"

renderArtifactPage :: Artifact -> String
renderArtifactPage artifact =
  let
    canonicalLink =
      if artifact.source.itemUrl == "" then ""
      else "<a class=\"source-link\" href=\"" <> escapeHtml artifact.source.itemUrl <> "\">"
        <> "Vatican News"
        <> "</a>"

    firstReading = Array.find (\r -> r.kind == firstReadingKind) artifact.readings
    gospelReading = Array.find (\r -> r.kind == gospelKind) artifact.readings

    readingHtml = case firstReading of
      Nothing -> ""
      Just r -> renderReadingBox r

    gospelHtml = case gospelReading of
      Nothing -> ""
      Just r -> renderReadingBox r

    translationNote = case Array.head artifact.readings of
      Nothing -> ""
      Just r -> r.translation.name <> " (" <> r.translation.id <> ")"

    hasLlm = Array.length artifact.llm.calls > 0
  in
    "<!doctype html>"
      <> "<html lang=\"en\">"
      <> "<head>"
      <> "<meta charset=\"utf-8\" />"
      <> "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />"
      <> "<title>Verbum Diei — " <> escapeHtml artifact.date <> "</title>"
      <> "<link rel=\"stylesheet\" href=\"/styles.css\" />"
      <> "</head>"
      <> "<body>"
      <> "<main class=\"layout\">"
      <> "<header class=\"page-header\">"
      <> "<div class=\"page-title\">Verbum Diei</div>"
      <> "<div class=\"page-meta\">"
      <> "<span class=\"page-date\">" <> escapeHtml artifact.date <> "</span>"
      <> canonicalLink
      <> "</div>"
      <> "</header>"
      <> renderObservances artifact
      <> "<aside class=\"box marginalia-box\">"
      <> "<header class=\"section-header\">"
      <> "<div class=\"section-kicker\">Margin</div>"
      <> "<h2 class=\"section-title\">Marginalia</h2>"
      <> "</header>"
      <> renderMarginalia hasLlm artifact.marginalia
      <> "</aside>"
      <> "<div class=\"reading-area\">"
      <> readingHtml
      <> "</div>"
      <> "<div class=\"gospel-area\">"
      <> gospelHtml
      <> "</div>"
      <> renderCommentaryBox hasLlm artifact
      <> "<footer class=\"page-footer\">"
      <> "<div class=\"footer-note\">Scripture text: " <> escapeHtml translationNote <> "</div>"
      <> "</footer>"
      <> "</main>"
      <> "</body>"
      <> "</html>"
