module VerbumDiei.Site
  ( renderArtifactPage
  , renderArchivePage
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String as String
import VerbumDiei.Artifact (Artifact, CommentNote, MarginalNote, Reading, ReadingKind, firstReadingKind, gospelKind)

type RenderConfig =
  { assetPrefix :: String
  , homeHref :: String
  , archiveHref :: String
  , permalinkHref :: String
  }

type ArchiveConfig =
  { assetPrefix :: String
  , homeHref :: String
  , dayHrefPrefix :: String
  }

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
      hl = String.joinWith " " (note.lines <#> \n -> lineId note.readingKind n)
    in
      "<li class=\"note\">"
        <> "<a class=\"note-ref\" data-hl=\"" <> escapeHtml hl <> "\" href=\"" <> escapeHtml href <> "\">"
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
      hl = String.joinWith " " (note.lines <#> \n -> lineId kind n)
    in
      "<li class=\"comment-note\">"
        <> "<a class=\"note-ref\" data-hl=\"" <> escapeHtml hl <> "\" href=\"" <> escapeHtml href <> "\">"
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

    excursusText = String.trim artifact.commentary.excursus

    excursusBody =
      if excursusText == "" then
        if hasLlm then "<div class=\"empty\">(no excursus generated)</div>"
        else "<div class=\"empty\">LLM output unavailable. Set <code>OPENAI_API_KEY</code> and re-run <code>bun run generate</code>.</div>"
      else "<div class=\"excursus-text\">" <> escapeHtml excursusText <> "</div>"

    seminaText = String.trim artifact.commentary.seminaVerbi

    seminaBody =
      if seminaText == "" then
        if hasLlm then "<div class=\"empty\">(no semina verbi generated)</div>"
        else "<div class=\"empty\">LLM output unavailable. Set <code>OPENAI_API_KEY</code> and re-run <code>bun run generate</code>.</div>"
      else "<div class=\"semina-verbi-text\">" <> escapeHtml seminaText <> "</div>"
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
      <> "<div class=\"excursus\">"
      <> "<div class=\"section-kicker\">Excursus</div>"
      <> excursusBody
      <> "</div>"
      <> "<div class=\"semina-verbi\">"
      <> "<div class=\"section-kicker\">Semina Verbi</div>"
      <> seminaBody
      <> "</div>"
      <> "</section>"

renderArtifactPage :: RenderConfig -> Artifact -> String
renderArtifactPage config artifact =
  let
    navLinks =
      String.joinWith "" $
        Array.catMaybes
          [ if config.homeHref == "" then Nothing else Just (navLink config.homeHref "Latest")
          , if config.archiveHref == "" then Nothing else Just (navLink config.archiveHref "Archive")
          , if config.permalinkHref == "" then Nothing else Just (navLink config.permalinkHref "Permalink")
          ]

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
      <> "<link rel=\"icon\" href=\"" <> escapeHtml (config.assetPrefix <> "favicon.ico") <> "\" />"
      <> "<link rel=\"icon\" type=\"image/png\" sizes=\"32x32\" href=\"" <> escapeHtml (config.assetPrefix <> "favicon-32x32.png") <> "\" />"
      <> "<link rel=\"icon\" type=\"image/png\" sizes=\"16x16\" href=\"" <> escapeHtml (config.assetPrefix <> "favicon-16x16.png") <> "\" />"
      <> "<link rel=\"apple-touch-icon\" sizes=\"180x180\" href=\"" <> escapeHtml (config.assetPrefix <> "apple-touch-icon.png") <> "\" />"
      <> "<link rel=\"stylesheet\" href=\"" <> escapeHtml (config.assetPrefix <> "styles.css") <> "\" />"
      <> "</head>"
      <> "<body>"
      <> "<main class=\"layout\">"
      <> "<header class=\"page-header\">"
      <> "<div class=\"page-title\">Verbum Diei</div>"
      <> "<div class=\"page-meta\">"
      <> "<span class=\"page-date\">" <> escapeHtml artifact.date <> "</span>"
      <> canonicalLink
      <> navLinks
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
      <> highlightScript
      <> "</body>"
      <> "</html>"

navLink :: String -> String -> String
navLink href label =
  "<a class=\"source-link\" href=\"" <> escapeHtml href <> "\">"
    <> escapeHtml label
    <> "</a>"

highlightScript :: String
highlightScript =
  "<script>(function(){const CLS='is-highlighted';let els=[];function clear(){for(const el of els){el.classList.remove(CLS);}els=[];}function apply(ids){clear();for(const id of ids){const el=document.getElementById(id);if(el){el.classList.add(CLS);els.push(el);}}}document.addEventListener('click',function(ev){const a=ev.target&&ev.target.closest?ev.target.closest('a.note-ref'):null;if(!a)return;const raw=a.getAttribute('data-hl');if(!raw)return;const ids=raw.split(/\\s+/).filter(Boolean);if(ids.length)apply(ids);},true);})();</script>"

renderArchivePage :: ArchiveConfig -> Array String -> String
renderArchivePage config dates =
  let
    dateLinks =
      String.joinWith "" $
        dates <#> \d ->
          "<li class=\"archive-item\">"
            <> "<a class=\"source-link\" href=\"" <> escapeHtml (config.dayHrefPrefix <> d <> "/") <> "\">"
            <> escapeHtml d
            <> "</a>"
            <> "</li>"
  in
    "<!doctype html>"
      <> "<html lang=\"en\">"
      <> "<head>"
      <> "<meta charset=\"utf-8\" />"
      <> "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" />"
      <> "<title>Verbum Diei — Archive</title>"
      <> "<link rel=\"icon\" href=\"" <> escapeHtml (config.assetPrefix <> "favicon.ico") <> "\" />"
      <> "<link rel=\"icon\" type=\"image/png\" sizes=\"32x32\" href=\"" <> escapeHtml (config.assetPrefix <> "favicon-32x32.png") <> "\" />"
      <> "<link rel=\"icon\" type=\"image/png\" sizes=\"16x16\" href=\"" <> escapeHtml (config.assetPrefix <> "favicon-16x16.png") <> "\" />"
      <> "<link rel=\"apple-touch-icon\" sizes=\"180x180\" href=\"" <> escapeHtml (config.assetPrefix <> "apple-touch-icon.png") <> "\" />"
      <> "<link rel=\"stylesheet\" href=\"" <> escapeHtml (config.assetPrefix <> "styles.css") <> "\" />"
      <> "</head>"
      <> "<body>"
      <> "<main class=\"layout layout-single\">"
      <> "<header class=\"page-header\">"
      <> "<div class=\"page-title\">Verbum Diei</div>"
      <> "<div class=\"page-meta\">"
      <> "<span class=\"page-date\">Archive</span>"
      <> (if config.homeHref == "" then "" else navLink config.homeHref "Latest")
      <> "</div>"
      <> "</header>"
      <> "<section class=\"box archive-box\">"
      <> "<header class=\"section-header\">"
      <> "<div class=\"section-kicker\">All Days</div>"
      <> "<h2 class=\"section-title\">Archive</h2>"
      <> "</header>"
      <> "<ul class=\"archive-list\">"
      <> dateLinks
      <> "</ul>"
      <> "</section>"
      <> "<footer class=\"page-footer\">"
      <> "<div class=\"footer-note\">Generated daily.</div>"
      <> "</footer>"
      <> "</main>"
      <> "</body>"
      <> "</html>"
