module VerbumDiei.Site
  ( renderArtifactPage
  , renderArchivePage
  , renderAppShellPage
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Effect (Effect)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Flame.Types (NodeData)
import VerbumDiei.Artifact (Artifact, CommentNote, HourEntry, MarginalNote, Reading, ReadingKind, firstReadingKind, gospelKind)

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

type AppShellConfig =
  { assetPrefix :: String
  , pageTitle :: String
  , defaultView :: String
  }

renderArtifactPage :: RenderConfig -> Artifact -> Effect String
renderArtifactPage config artifact =
  FRS.render (artifactDocument config artifact)

renderArchivePage :: ArchiveConfig -> Array String -> Effect String
renderArchivePage config dates =
  FRS.render (archiveDocument config dates)

renderAppShellPage :: AppShellConfig -> Effect String
renderAppShellPage config =
  FRS.render (appShellDocument config)

el :: forall message. String -> Array (NodeData message) -> Array (Html message) -> Html message
el = HE.createElement

leaf :: forall message. String -> Array (NodeData message) -> Html message
leaf = HE.createElement'

txt :: forall message. String -> Html message
txt = HE.text

-- Inject literal HTML (used for SVG ornaments). Wrapped in a contents-display
-- span so layout flows as if the markup were a sibling of the wrapper.
raw :: forall message. String -> Html message
raw markup =
  HE.createElement "span"
    [ HA.innerHtml markup
    , HA.styleAttr "display:contents"
    ]
    []

-- ------------------------------------------------------------
-- helpers
-- ------------------------------------------------------------

kindShort :: ReadingKind -> String
kindShort k =
  if k == gospelKind then "G" else "R"

kindLatin :: ReadingKind -> String
kindLatin k =
  if k == gospelKind then "Evangelium" else "Lectio"

kindKicker :: ReadingKind -> String
kindKicker k =
  if k == gospelKind then "Evangelium" else "Lectio Prima"

translationHref :: String -> String
translationHref = case _ of
  "dra" -> "https://www.gutenberg.org/ebooks/8300"
  _ -> "https://www.gutenberg.org/"

lineId :: ReadingKind -> Int -> String
lineId kind n =
  kind <> "-L" <> show n

lineLabelForReading :: Reading -> Int -> String
lineLabelForReading reading n =
  fromMaybe (show n) (Array.head (Array.drop (n - 1) reading.lineRefs))

lineLabelForKind :: Array Reading -> ReadingKind -> Int -> String
lineLabelForKind readings kind n =
  case Array.find (\r -> r.kind == kind) readings of
    Nothing -> show n
    Just r -> lineLabelForReading r n

noteTarget :: ReadingKind -> Array Int -> String
noteTarget kind lines =
  case Array.head lines of
    Nothing -> "#"
    Just n -> "#" <> lineId kind n

noteHighlights :: ReadingKind -> Array Int -> String
noteHighlights kind lines =
  String.joinWith " " (lines <#> \n -> lineId kind n)

renderLinesLabel :: (ReadingKind -> Int -> String) -> ReadingKind -> Array Int -> String
renderLinesLabel lineLabel kind lines =
  kindShort kind <> " " <> String.joinWith "," (lines <#> lineLabel kind)

paletteFromSeason :: String -> String
paletteFromSeason season =
  let
    s = String.toLower season
  in
    if String.contains (String.Pattern "lent") s then "lent"
    else if String.contains (String.Pattern "easter") s then "easter"
    else if String.contains (String.Pattern "advent") s then "advent"
    else if String.contains (String.Pattern "christmas") s then "christmas"
    else if String.contains (String.Pattern "ordinary") s then "ordinary"
    else "vespers"

hourLatin :: String -> String
hourLatin = case _ of
  "matins" -> "Ad Matutinum"
  "lauds" -> "Ad Laudes"
  "terce" -> "Ad Tertiam"
  "sext" -> "Ad Sextam"
  "none" -> "Ad Nonam"
  "vespers" -> "Ad Vesperas"
  "compline" -> "Ad Completorium"
  _ -> ""

pad2 :: Int -> String
pad2 n = if n < 10 then "0" <> show n else show n

toRomanNumeral :: Int -> String
toRomanNumeral n0 =
  let
    pairs =
      [ { s: "M", v: 1000 }
      , { s: "CM", v: 900 }
      , { s: "D", v: 500 }
      , { s: "CD", v: 400 }
      , { s: "C", v: 100 }
      , { s: "XC", v: 90 }
      , { s: "L", v: 50 }
      , { s: "XL", v: 40 }
      , { s: "X", v: 10 }
      , { s: "IX", v: 9 }
      , { s: "V", v: 5 }
      , { s: "IV", v: 4 }
      , { s: "I", v: 1 }
      ]
    go n acc =
      case Array.uncons (Array.filter (\p -> p.v <= n) pairs) of
        Nothing -> acc
        Just { head: p } ->
          if n <= 0 then acc
          else go (n - p.v) (acc <> p.s)
  in
    if n0 <= 0 then "" else go n0 ""

romanDate :: String -> String
romanDate dateIso =
  case String.split (String.Pattern "-") dateIso of
    [ y, m, d ] ->
      let
        yi = parseDigits y
        mi = parseDigits m
        di = parseDigits d
      in
        String.toLower (toRomanNumeral di)
          <> " · "
          <> String.toLower (toRomanNumeral mi)
          <> " · "
          <> toRomanNumeral yi
    _ -> dateIso
  where
  parseDigits s =
    fromMaybe 0 (Array.foldl step (Just 0) (CodeUnits.toCharArray s))
  step acc c =
    let
      code = case c of
        '0' -> Just 0
        '1' -> Just 1
        '2' -> Just 2
        '3' -> Just 3
        '4' -> Just 4
        '5' -> Just 5
        '6' -> Just 6
        '7' -> Just 7
        '8' -> Just 8
        '9' -> Just 9
        _ -> Nothing
    in
      do
        a <- acc
        v <- code
        pure (a * 10 + v)

navPill :: forall message. String -> String -> Html message
navPill href label =
  el "a"
    [ HA.class' "nav-pill"
    , HA.href href
    , HA.createAttribute "rel" "noreferrer"
    ]
    [ txt label ]

documentHead :: forall message. String -> String -> Html message
documentHead assetPrefix pageTitle =
  el "head" []
    [ leaf "meta" [ HA.charset "utf-8" ]
    , leaf "meta" [ HA.name "viewport", HA.content "width=device-width, initial-scale=1" ]
    , el "title" [] [ txt pageTitle ]
    , leaf "link" [ HA.rel "icon", HA.href (assetPrefix <> "favicon.ico") ]
    , leaf "link" [ HA.rel "icon", HA.type' "image/png", HA.createAttribute "sizes" "32x32", HA.href (assetPrefix <> "favicon-32x32.png") ]
    , leaf "link" [ HA.rel "icon", HA.type' "image/png", HA.createAttribute "sizes" "16x16", HA.href (assetPrefix <> "favicon-16x16.png") ]
    , leaf "link" [ HA.rel "apple-touch-icon", HA.createAttribute "sizes" "180x180", HA.href (assetPrefix <> "apple-touch-icon.png") ]
    , leaf "link" [ HA.rel "preconnect", HA.href "https://fonts.googleapis.com" ]
    , leaf "link" [ HA.rel "preconnect", HA.href "https://fonts.gstatic.com", HA.createAttribute "crossorigin" "" ]
    , leaf "link"
        [ HA.rel "stylesheet"
        , HA.href "https://fonts.googleapis.com/css2?family=EB+Garamond:ital,wght@0,400;0,500;0,600;0,700;1,400;1,500&family=Cormorant+Unicase:wght@400;500;600;700&family=UnifrakturCook:wght@700&family=IBM+Plex+Mono:wght@400;500;600&display=swap"
        ]
    , leaf "link" [ HA.rel "stylesheet", HA.href (assetPrefix <> "styles.css") ]
    ]

-- ------------------------------------------------------------
-- ornaments
-- ------------------------------------------------------------

fleuronSvg :: forall message. String -> Html message
fleuronSvg pos =
  raw $
    "<svg class=\"fleuron " <> pos <> "\" viewBox=\"0 0 56 56\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.2\" aria-hidden=\"true\">"
      <> "<path d=\"M4 4 Q 14 4, 14 14 Q 14 24, 24 24\"/>"
      <> "<path d=\"M4 4 Q 4 14, 14 14\"/>"
      <> "<circle cx=\"14\" cy=\"14\" r=\"2.2\" fill=\"currentColor\" stroke=\"none\"/>"
      <> "<path d=\"M28 8 Q 22 14, 28 22 Q 34 14, 28 8 Z\" fill=\"currentColor\" opacity=\"0.6\" stroke=\"none\"/>"
      <> "<path d=\"M8 28 Q 14 22, 22 28 Q 14 34, 8 28 Z\" fill=\"currentColor\" opacity=\"0.6\" stroke=\"none\"/>"
      <> "<path d=\"M28 28 m -3 0 a 3 3 0 1 0 6 0 a 3 3 0 1 0 -6 0\" fill=\"currentColor\" stroke=\"none\"/>"
      <> "</svg>"

saintMarkSvg :: forall message. Html message
saintMarkSvg =
  raw $
    "<svg class=\"saint-mark\" viewBox=\"0 0 32 32\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.4\" aria-hidden=\"true\">"
      <> "<path d=\"M16 3 L16 29 M9 11 L23 11 M11 22 L21 22\"/>"
      <> "<circle cx=\"16\" cy=\"7\" r=\"2.4\" fill=\"currentColor\" stroke=\"none\"/>"
      <> "</svg>"

calToggleSvg :: forall message. Html message
calToggleSvg =
  raw $
    "<svg viewBox=\"0 0 12 12\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.2\" aria-hidden=\"true\">"
      <> "<rect x=\"1\" y=\"2\" width=\"10\" height=\"9\"/>"
      <> "<line x1=\"1\" y1=\"5\" x2=\"11\" y2=\"5\"/>"
      <> "<line x1=\"4\" y1=\"2\" x2=\"4\" y2=\"0.5\"/>"
      <> "<line x1=\"8\" y1=\"2\" x2=\"8\" y2=\"0.5\"/>"
      <> "</svg>"

-- ------------------------------------------------------------
-- sections
-- ------------------------------------------------------------

renderColophon :: forall message. RenderConfig -> Artifact -> Html message
renderColophon config artifact =
  let
    sourceUrl = artifact.source.itemUrl
    pills =
      Array.catMaybes
        [ if sourceUrl == "" then Nothing else Just (navPill sourceUrl "Vatican")
        , if config.archiveHref == "" then Nothing else Just (navPill config.archiveHref "Archive")
        , if config.homeHref == "" then Nothing else Just (navPill config.homeHref "Latest")
        ]
  in
    el "header" [ HA.class' "colophon" ]
      [ el "div" [ HA.class' "colophon-left" ]
          [ el "span" [] [ txt "Codex Verbi · Folio" ]
          , el "span" [ HA.styleAttr "color: var(--rubric)" ] [ txt "Scriptura · Oratio · Glossa" ]
          ]
      , el "div" [ HA.class' "wordmark" ]
          [ el "h1" [ HA.class' "v-d" ] [ txt "Verbum Diei" ]
          , el "div" [ HA.class' "latin" ] [ txt "Lectionarium Cottidianum" ]
          ]
      , el "div" [ HA.class' "colophon-right" ]
          [ el "span"
              [ HA.class' "date-long"
              , HA.createAttribute "data-iso" artifact.date
              ]
              [ txt artifact.date ]
          , el "span" [ HA.class' "date-roman" ] [ txt (romanDate artifact.date) ]
          , el "span" [ HA.class' "nav-pills" ] pills
          ]
      ]

renderObservances :: forall message. Artifact -> Html message
renderObservances artifact =
  let
    meta = artifact.observances.meta
    metaHasContent = meta.season /= "" || meta.cycle /= "" || meta.psalterWeek /= ""
    metaRow =
      if not metaHasContent then []
      else
        [ el "div" [ HA.class' "observance-row" ]
            [ el "div" [ HA.class' "observance-cell" ]
                [ el "div" [ HA.class' "lab" ] [ txt "Tempus" ]
                , el "div" [ HA.class' "val" ] [ txt (orDash meta.season) ]
                ]
            , el "div" [ HA.class' "observance-cell" ]
                [ el "div" [ HA.class' "lab" ] [ txt "Cyclus" ]
                , el "div" [ HA.class' "val" ] [ txt (orDash meta.cycle) ]
                ]
            , el "div" [ HA.class' "observance-cell" ]
                [ el "div" [ HA.class' "lab" ] [ txt "Psalterium" ]
                , el "div" [ HA.class' "val" ] [ txt (orDash meta.psalterWeek) ]
                ]
            ]
        ]

    celebrationNodes =
      artifact.observances.celebrations <#> \c ->
        el "div" [ HA.class' "celebration" ]
          [ saintMarkSvg
          , el "span" [ HA.class' "rank" ] [ txt (orDash c.rank) ]
          , el "span" [ HA.class' "name" ] [ txt c.name ]
          ]
  in
    el "section" [ HA.class' "section", HA.id "observances" ]
      ([ el "h2" [ HA.class' "rubric-heading" ]
            [ el "span" [ HA.class' "num" ] [ txt "I" ]
            , el "span" [ HA.class' "title-main" ] [ txt "Observantiæ" ]
            , el "span" [ HA.class' "latin" ] [ txt "Dies Hodierna" ]
            ]
       ] <> metaRow <> celebrationNodes)
  where
  orDash s = if s == "" then "—" else s

-- Hours: render sun-arc placeholder + initial hour-card for each hour.
-- JS will hide all but the current/active card and animate the sun.
renderHours :: forall message. Array HourEntry -> Html message
renderHours rawHours =
  let
    rows = if Array.null rawHours then defaultHours else rawHours
    arcSvg =
      raw $
        "<svg class=\"clock-arc\" viewBox=\"0 0 760 240\" preserveAspectRatio=\"xMidYMid meet\" data-hours-arc=\"true\" aria-hidden=\"true\">"
          <> "<line class=\"horizon\" x1=\"15\" y1=\"216\" x2=\"745\" y2=\"216\"/>"
          <> "<path class=\"arc-line\" d=\"M 80 216 A 300 170 0 0 1 680 216\"/>"
          <> "<path class=\"arc-day\" d=\"M 80 216 A 300 170 0 0 1 680 216\"/>"
          <> "</svg>"

    hourCard h =
      el "div"
        [ HA.class' "hour-card"
        , HA.createAttribute "data-hour-key" h.key
        , HA.createAttribute "data-hour-local" (show h.hourLocal)
        , HA.createAttribute "data-minute-local" (show h.minuteLocal)
        , HA.createAttribute "data-hour-source" h.source
        ]
        [ el "div" [ HA.class' "head" ]
            [ el "span" [ HA.class' "label" ] [ txt h.label ]
            , el "span" [ HA.class' "latin-name" ] [ txt (hourLatin h.key) ]
            , el "span" [ HA.class' "time" ]
                [ txt (pad2 h.hourLocal <> ":" <> pad2 h.minuteLocal) ]
            ]
        , el "div" [ HA.class' "prayer" ] [ txt h.prayer ]
        ]
  in
    el "section" [ HA.class' "section", HA.id "hours-of-prayer" ]
      [ el "h2" [ HA.class' "rubric-heading" ]
          [ el "span" [ HA.class' "num" ] [ txt "II" ]
          , el "span" [ HA.class' "title-main" ] [ txt "Horæ Canonicæ" ]
          , el "span" [ HA.class' "latin" ] [ txt "Officium Divinum" ]
          ]
      , el "div" [ HA.class' "section-meta" ]
          [ txt "Seven stations · the sun above traces the day · click a station to read its prayer." ]
      , el "div" [ HA.class' "hours-wrap" ] [ arcSvg ]
      , el "div"
          [ HA.id "hours-cards"
          , HA.createAttribute "data-hours-list" "true"
          ]
          (rows <#> hourCard)
      ]

defaultHours :: Array HourEntry
defaultHours =
  [ { key: "matins", label: "Matins", hourLocal: 0, minuteLocal: 0, prayer: "Lord, open my lips, and my mouth shall declare your praise.", source: "fallback" }
  , { key: "lauds", label: "Lauds", hourLocal: 6, minuteLocal: 0, prayer: "Blessed are you, Lord, in the light of the new day.", source: "fallback" }
  , { key: "terce", label: "Terce", hourLocal: 9, minuteLocal: 0, prayer: "Come, Holy Spirit, and lighten our work in truth.", source: "fallback" }
  , { key: "sext", label: "Sext", hourLocal: 12, minuteLocal: 0, prayer: "God, come to my assistance. Lord, make haste to help me.", source: "fallback" }
  , { key: "none", label: "Nones", hourLocal: 15, minuteLocal: 0, prayer: "Stay with us, Lord, in the heat and trial of this day.", source: "fallback" }
  , { key: "vespers", label: "Vespers", hourLocal: 18, minuteLocal: 0, prayer: "Let my prayer rise before you like incense this evening.", source: "fallback" }
  , { key: "compline", label: "Compline", hourLocal: 21, minuteLocal: 0, prayer: "Into your hands, Lord, I commend my spirit.", source: "fallback" }
  ]

renderReading
  :: forall message
   . Boolean
  -> Int
  -> Reading
  -> Array MarginalNote
  -> Html message
renderReading dropcap idx reading marginalia =
  let
    notesForKind = Array.filter (\n -> n.readingKind == reading.kind) marginalia
    notesByLineIdx = \i ->
      Array.find (\n -> Array.elem (i + 1) n.lines) notesForKind

    verseRows =
      reading.lines # Array.mapWithIndex \i lineText ->
        let
          n = i + 1
          id_ = lineId reading.kind n
          label = lineLabelForReading reading n
          maybeNote = notesByLineIdx i
          hasMargin = case maybeNote of
            Nothing -> "false"
            Just _ -> "true"
          verseEl =
            el "p"
              [ HA.class' "verse"
              , HA.id id_
              , HA.createAttribute "data-verse-id" id_
              , HA.createAttribute "data-has-margin" hasMargin
              , HA.createAttribute "data-line-ref" label
              ]
              [ el "span" [ HA.class' "vn" ] [ txt label ]
              , el "span" [ HA.class' "vt" ] [ txt lineText ]
              ]
          gutterChildren = case maybeNote of
            Nothing -> []
            Just note ->
              [ el "aside"
                  [ HA.class' "gutter-note"
                  , HA.createAttribute "data-gutter-for" id_
                  ]
                  [ el "span" [ HA.class' "manicule" ] [ txt "☞" ]
                  , el "span" [ HA.class' "ref-tag" ]
                      [ txt (kindShort reading.kind <> " " <> label) ]
                  , el "div" [] [ txt note.text ]
                  ]
              ]
        in
          el "div" [ HA.class' "verse-row" ] ([ verseEl ] <> gutterChildren)

    panelId = if reading.kind == gospelKind then "reading-gospel" else "reading-first"
    numLabel = if idx == 0 then "III" else "IV"
    classes =
      if dropcap then "scripture with-dropcap" else "scripture"
  in
    el "section"
      [ HA.class' "section reading"
      , HA.id panelId
      ]
      [ el "h2" [ HA.class' "rubric-heading" ]
          [ el "span" [ HA.class' "num" ] [ txt numLabel ]
          , el "span" [ HA.class' "title-main" ] [ txt (kindLatin reading.kind) ]
          , el "span" [ HA.class' "latin" ] [ txt reading.reference ]
          ]
      , el "div" [ HA.class' "reading-head" ]
          [ el "div" [ HA.class' "kicker" ] [ txt (kindKicker reading.kind) ]
          , el "h3" [ HA.class' "heading" ] [ txt reading.heading ]
          , el "div" [ HA.class' "ref" ]
              [ txt (reading.reference <> " · " <> reading.translation.name) ]
          ]
      , el "div" [ HA.class' classes ] verseRows
      ]

renderCommentary
  :: forall message
   . Boolean
  -> (ReadingKind -> Int -> String)
  -> Artifact
  -> Html message
renderCommentary hasLlm lineLabel artifact =
  let
    c = artifact.commentary
    isEmpty =
      Array.length c.reading == 0
        && Array.length c.gospel == 0
        && c.synthesis == ""
        && c.excursus == ""
        && c.seminaVerbi == ""

    emptyText =
      if hasLlm then "(no commentary generated)"
      else "LLM output unavailable. Set OPENAI_API_KEY and re-run bun run generate."

    synthesisNode =
      if String.trim c.synthesis == "" then []
      else
        [ el "div" [ HA.class' "synthesis" ]
            [ el "span" [ HA.class' "lead" ] [ txt "Sententia Doctrinalis" ]
            , txt c.synthesis
            ]
        ]

    commentNoteItem :: ReadingKind -> CommentNote -> Html message
    commentNoteItem kind note =
      el "li" []
        [ el "a"
            [ HA.class' "note-ref"
            , HA.href (noteTarget kind note.lines)
            , HA.createAttribute "data-hl" (noteHighlights kind note.lines)
            ]
            [ txt (renderLinesLabel lineLabel kind note.lines) ]
        , el "span" [ HA.class' "note-text" ] [ txt note.text ]
        ]

    columnNode kind heading notes =
      el "section" [ HA.class' "commentary-col" ]
        [ el "h4" [] [ txt heading ]
        , if Array.length notes == 0 then
            el "div" [ HA.class' "empty-note" ] [ txt emptyText ]
          else
            el "ul" [] (notes <#> commentNoteItem kind)
        ]

    columnsNode =
      if Array.length c.reading == 0 && Array.length c.gospel == 0 then []
      else
        [ el "div" [ HA.class' "commentary-cols" ]
            [ columnNode firstReadingKind "In Lectionem" c.reading
            , columnNode gospelKind "In Evangelium" c.gospel
            ]
        ]

    longProse cls heading bodyText =
      let
        bodyTrim = String.trim bodyText
        paragraphs =
          if bodyTrim == "" then [ emptyText ]
          else String.split (String.Pattern "\n\n") bodyTrim
      in
        el "section" [ HA.class' cls ]
          ([ el "h4" [] [ txt heading ] ]
            <> (paragraphs <#> \p -> el "p" [] [ txt p ]))

    proseClass enabled =
      if enabled then "long-prose with-dropcap" else "long-prose"

    excursusNode =
      if String.trim c.excursus == "" && not hasLlm then []
      else [ longProse (proseClass true) "Lectio Heterodoxa" c.excursus ]

    seminaNode =
      if String.trim c.seminaVerbi == "" && not hasLlm then []
      else [ longProse "long-prose" "Semina Verbi" c.seminaVerbi ]

    body =
      if isEmpty then [ el "div" [ HA.class' "empty-note" ] [ txt emptyText ] ]
      else
        synthesisNode
          <> columnsNode
          <> excursusNode
          <> seminaNode
  in
    el "section" [ HA.class' "section", HA.id "commentary" ]
      ([ el "h2" [ HA.class' "rubric-heading" ]
          [ el "span" [ HA.class' "num" ] [ txt "V" ]
          , el "span" [ HA.class' "title-main" ] [ txt "Glossa" ]
          , el "span" [ HA.class' "latin" ] [ txt "Commentarium" ]
          ]
       ] <> body)

renderGutterRail :: forall message. Artifact -> Html message
renderGutterRail artifact =
  let
    meta = artifact.observances.meta
    firstCelebration = Array.head artifact.observances.celebrations
    daySummary = String.trim artifact.commentary.synthesis

    metaItem lab v =
      if v == "" then Nothing
      else
        Just $
          el "li" []
            [ el "span" [ HA.class' "lab" ] [ txt lab ]
            , txt v
            ]

    celebrationItem =
      case firstCelebration of
        Nothing -> Nothing
        Just c ->
          Just $
            el "li" []
              [ el "span" [ HA.class' "lab" ] [ txt "Sanctus" ]
              , txt c.name
              ]

    sourceItem =
      if artifact.source.itemUrl == "" then Nothing
      else
        Just $
          el "li" []
            [ el "span" [ HA.class' "lab" ] [ txt "Fons" ]
            , el "a" [ HA.href artifact.source.itemUrl ] [ txt "Vatican News" ]
            ]

    contextItems =
      Array.catMaybes
        [ metaItem "Tempus" meta.season
        , metaItem "Cyclus" meta.cycle
        , metaItem "Psalter" meta.psalterWeek
        , celebrationItem
        , sourceItem
        ]

    briefNode =
      if daySummary == "" then []
      else
        [ el "h4" [] [ txt "Sententia" ]
        , el "p" [ HA.class' "day-brief" ] [ txt daySummary ]
        ]
  in
    el "div" [ HA.class' "gutter-rail" ]
      ([ el "h4" [] [ txt "In Hac Pagina" ]
       , el "ul" []
            [ el "li" [] [ el "a" [ HA.href "#observances" ] [ txt "Observantiæ" ] ]
            , el "li" [] [ el "a" [ HA.href "#hours-of-prayer" ] [ txt "Horæ Canonicæ" ] ]
            , el "li" [] [ el "a" [ HA.href "#reading-first" ] [ txt "Lectio" ] ]
            , el "li" [] [ el "a" [ HA.href "#reading-gospel" ] [ txt "Evangelium" ] ]
            , el "li" [] [ el "a" [ HA.href "#commentary" ] [ txt "Glossa" ] ]
            ]
       ]
        <> briefNode
        <>
          [ el "h4" [] [ txt "Day Context" ]
          , el "ul" [ HA.class' "meta-list" ] contextItems
          ])

renderCalPeek :: forall message. String -> Html message
renderCalPeek archiveHref =
  el "div"
    [ HA.id "cal-peek"
    , HA.class' "cal-peek is-closed"
    , HA.createAttribute "data-cal-peek" "true"
    ]
    [ el "h5" [] [ txt "Calendarium" ]
    , el "ul" [ HA.id "cal-peek-list" ] []
    , el "div" [ HA.class' "cal-foot" ]
        [ el "a" [ HA.href archiveHref ] [ txt "All Days →" ] ]
    ]

renderCalToggle :: forall message. Html message
renderCalToggle =
  el "button"
    [ HA.class' "cal-toggle"
    , HA.id "cal-toggle"
    , HA.type' "button"
    , HA.createAttribute "aria-controls" "cal-peek"
    , HA.createAttribute "aria-expanded" "false"
    ]
    [ calToggleSvg
    , txt "Calendarium"
    ]

renderFooter :: forall message. Reading -> Artifact -> Html message
renderFooter firstReading _artifact =
  el "footer" [ HA.class' "codex-footer" ]
    [ el "div" []
        [ txt "Scripturæ textus: "
        , el "a"
            [ HA.href (translationHref firstReading.translation.id) ]
            [ txt (firstReading.translation.name <> " · Public Domain") ]
        ]
    , el "div" []
        [ txt "Calendarium per "
        , el "a" [ HA.href "https://github.com/romcal/romcal" ] [ txt "romcal" ]
        , txt " · Horarum textus per "
        , el "a" [ HA.href "https://github.com/DavidLara/breviarium" ] [ txt "breviarium" ]
        ]
    , el "div" []
        [ txt "Marginalia et glossæ a machina linguistica generatæ — ad meditationem, non ad doctrinam." ]
    , el "div" []
        [ txt "Source: "
        , el "a" [ HA.href "https://github.com/brickfrog/verbum-diei" ]
            [ txt "github.com/brickfrog/verbum-diei" ]
        ]
    , el "div" [ HA.class' "explicit" ] [ txt "Explicit Liber · Deo Gratias" ]
    ]

-- ------------------------------------------------------------
-- documents
-- ------------------------------------------------------------

artifactDocument :: RenderConfig -> Artifact -> Html Unit
artifactDocument config artifact =
  let
    firstReading = Array.find (\r -> r.kind == firstReadingKind) artifact.readings
    gospelReading = Array.find (\r -> r.kind == gospelKind) artifact.readings
    lineLabel = lineLabelForKind artifact.readings
    hasLlm = Array.length artifact.llm.calls > 0
    palette = paletteFromSeason artifact.observances.meta.season
    fallbackReading =
      { kind: firstReadingKind
      , heading: ""
      , reference: ""
      , bibleApiReference: ""
      , translation: { id: "dra", name: "Douay-Rheims 1899", note: "" }
      , lineRefs: []
      , lines: []
      }
    firstForFooter = fromMaybe fallbackReading firstReading

    readingNodes =
      Array.catMaybes
        [ map (\r -> renderReading true 0 r artifact.marginalia) firstReading
        , map (\r -> renderReading true 1 r artifact.marginalia) gospelReading
        ]

    divider =
      el "div" [ HA.class' "divider-rule", HA.createAttribute "aria-hidden" "true" ]
        [ el "span" [ HA.class' "line" ] []
        , el "span" [ HA.class' "glyph" ] [ txt "✠ ❦ ✠" ]
        , el "span" [ HA.class' "line" ] []
        ]
  in
    el "html"
      [ HA.lang "en"
      , HA.createAttribute "data-palette" palette
      ]
      [ documentHead config.assetPrefix ("Verbum Diei · " <> artifact.date)
      , el "body" [ HA.class' "codex-body" ]
          [ renderCalToggle
          , renderCalPeek config.archiveHref
          , el "main" [ HA.class' "codex" ]
              [ el "article" [ HA.class' "codex-page" ]
                  ([ fleuronSvg "tl"
                   , fleuronSvg "tr"
                   , fleuronSvg "bl"
                   , fleuronSvg "br"
                   , renderColophon config artifact
                   , el "div" [ HA.class' "codex-grid placement-gutter" ]
                       [ el "div" [ HA.class' "center-column" ]
                           ([ renderObservances artifact
                            , renderHours artifact.hoursOfPrayer
                            , divider
                            ] <> readingNodes <>
                            [ renderCommentary hasLlm lineLabel artifact
                            , renderFooter firstForFooter artifact
                            ])
                       , el "aside" [ HA.class' "gutter-rail-col" ]
                           [ renderGutterRail artifact ]
                       ]
                   ])
              ]
          , leaf "script"
              [ HA.src (config.assetPrefix <> "app.js")
              , HA.createAttribute "defer" "defer"
              ]
          ]
      ]

archiveDocument :: ArchiveConfig -> Array String -> Html Unit
archiveDocument config dates =
  let
    items =
      dates <#> \d ->
        el "li" [ HA.class' "archive-item" ]
          [ el "a" [ HA.href (config.dayHrefPrefix <> d <> "/") ] [ txt d ] ]
    listOrEmpty =
      if Array.length items == 0 then
        el "div" [ HA.class' "empty-note" ] [ txt "No generated days yet." ]
      else
        el "ul" [ HA.class' "archive-list" ] items

    pills =
      if config.homeHref == "" then [] else [ navPill config.homeHref "Latest" ]
  in
    el "html"
      [ HA.lang "en"
      , HA.createAttribute "data-palette" "vespers"
      ]
      [ documentHead config.assetPrefix "Verbum Diei · Archive"
      , el "body" [ HA.class' "codex-body" ]
          [ el "main" [ HA.class' "codex" ]
              [ el "article" [ HA.class' "codex-page" ]
                  [ fleuronSvg "tl"
                  , fleuronSvg "tr"
                  , fleuronSvg "bl"
                  , fleuronSvg "br"
                  , el "header" [ HA.class' "colophon" ]
                      [ el "div" [ HA.class' "colophon-left" ]
                          [ el "span" [] [ txt "All Days · Chronicon" ] ]
                      , el "div" [ HA.class' "wordmark" ]
                          [ el "h1" [ HA.class' "v-d" ] [ txt "Archivum" ]
                          , el "div" [ HA.class' "latin" ] [ txt "Codex Dierum" ]
                          ]
                      , el "div" [ HA.class' "colophon-right" ]
                          [ el "span" [ HA.class' "nav-pills" ] pills ]
                      ]
                  , el "div" [ HA.class' "codex-grid", HA.styleAttr "grid-template-columns: 1fr;" ]
                      [ el "div" [ HA.class' "center-column" ]
                          [ el "section" [ HA.class' "section" ]
                              [ el "h2" [ HA.class' "rubric-heading" ]
                                  [ el "span" [ HA.class' "num" ] [ txt "I" ]
                                  , el "span" [ HA.class' "title-main" ] [ txt "Dies" ]
                                  , el "span" [ HA.class' "latin" ] [ txt "Omnia" ]
                                  ]
                              , listOrEmpty
                              ]
                          , el "footer" [ HA.class' "codex-footer" ]
                              [ el "div" [] [ txt "Generated daily." ]
                              , el "div" [ HA.class' "explicit" ] [ txt "Codex Dierum · Deo Gratias" ]
                              ]
                          ]
                      ]
                  ]
              ]
          ]
      ]

appShellDocument :: AppShellConfig -> Html Unit
appShellDocument config =
  el "html"
    [ HA.lang "en"
    , HA.createAttribute "data-palette" "vespers"
    ]
    [ documentHead config.assetPrefix config.pageTitle
    , el "body" [ HA.class' "codex-body app-shell-body" ]
        [ el "main"
            [ HA.id "app-root"
            , HA.class' "codex"
            , HA.createAttribute "data-default-view" config.defaultView
            , HA.createAttribute "data-asset-prefix" config.assetPrefix
            ]
            []
        , leaf "script" [ HA.src (config.assetPrefix <> "app.js"), HA.createAttribute "defer" "defer" ]
        ]
    ]
