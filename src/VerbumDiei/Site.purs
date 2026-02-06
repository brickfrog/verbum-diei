module VerbumDiei.Site
  ( renderArtifactPage
  , renderArchivePage
  , renderAppShellPage
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect (Effect)
import Flame (Html)
import Flame.Html.Attribute as HA
import Flame.Html.Element as HE
import Flame.Renderer.String as FRS
import Flame.Types (NodeData)
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

kindLabel :: ReadingKind -> String
kindLabel = case _ of
  k | k == gospelKind -> "Gospel"
  _ -> "Reading"

kindShort :: ReadingKind -> String
kindShort = case _ of
  k | k == gospelKind -> "G"
  _ -> "R"

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

navLink :: forall message. String -> String -> Html message
navLink href label =
  el "a" [ HA.class' "site-link", HA.href href ] [ txt label ]

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
        , HA.href "https://fonts.googleapis.com/css2?family=Source+Serif+4:ital,wght@0,400;0,600;0,700;1,400&family=Source+Sans+3:wght@400;600;700&display=swap"
        ]
    , leaf "link" [ HA.rel "stylesheet", HA.href (assetPrefix <> "styles.css") ]
    ]

renderLine :: forall message. ReadingKind -> Int -> String -> String -> Html message
renderLine kind n label lineText =
  el "p" [ HA.class' "scripture-line", HA.id (lineId kind n) ]
    [ el "span" [ HA.class' "line-label" ] [ txt label ]
    , el "span" [ HA.class' "line-text" ] [ txt lineText ]
    ]

renderReadingBox :: forall message. Reading -> Html message
renderReadingBox reading =
  el "section" [ HA.class' "panel reading-panel" ]
    [ el "header" [ HA.class' "panel-header" ]
        [ el "div" [ HA.class' "panel-kicker" ] [ txt (kindLabel reading.kind) ]
        , el "h2" [ HA.class' "panel-title" ] [ txt reading.heading ]
        , el "div" [ HA.class' "panel-ref" ] [ txt reading.reference ]
        , el "div" [ HA.class' "panel-meta" ] [ txt (reading.translation.name <> " - " <> reading.translation.note) ]
        ]
    , el "article" [ HA.class' "scripture-block" ]
        (reading.lines # Array.mapWithIndex \i line ->
          let
            n = i + 1
          in
            renderLine reading.kind n (lineLabelForReading reading n) line)
    ]

renderObservances :: forall message. Artifact -> Html message
renderObservances artifact =
  let
    metaNodes =
      if artifact.observances.meta.season == "" && artifact.observances.meta.cycle == "" && artifact.observances.meta.psalterWeek == "" then
        []
      else
        [ el "div" [ HA.class' "observance-meta-grid" ]
            [ el "div" [ HA.class' "meta-cell" ]
                [ el "span" [ HA.class' "meta-label" ] [ txt "Season" ]
                , el "span" [ HA.class' "meta-value" ] [ txt artifact.observances.meta.season ]
                ]
            , el "div" [ HA.class' "meta-cell" ]
                [ el "span" [ HA.class' "meta-label" ] [ txt "Cycle" ]
                , el "span" [ HA.class' "meta-value" ] [ txt artifact.observances.meta.cycle ]
                ]
            , el "div" [ HA.class' "meta-cell" ]
                [ el "span" [ HA.class' "meta-label" ] [ txt "Psalter" ]
                , el "span" [ HA.class' "meta-value" ] [ txt artifact.observances.meta.psalterWeek ]
                ]
            ]
        ]

    celebrationNodes =
      artifact.observances.celebrations <#> \c ->
        el "li" [ HA.class' "celebration-item" ]
          [ el "span" [ HA.class' "celebration-rank" ] [ txt c.rank ]
          , el "span" [ HA.class' "celebration-name" ] [ txt c.name ]
          ]
  in
    el "section" [ HA.class' "panel observances-panel" ]
      ([ el "header" [ HA.class' "panel-header" ]
          [ el "div" [ HA.class' "panel-kicker" ] [ txt "Day Office" ]
          , el "h2" [ HA.class' "panel-title" ] [ txt "Observances" ]
          ]
       ]
        <> metaNodes
        <> [ el "ul" [ HA.class' "celebration-list" ] celebrationNodes ])

renderMarginalia :: forall message. (ReadingKind -> Int -> String) -> Boolean -> Array MarginalNote -> Html message
renderMarginalia lineLabel hasLlm notes =
  if Array.length notes == 0 then
    el "div" [ HA.class' "empty-note" ]
      [ txt $
          if hasLlm then
            "(no marginalia generated)"
          else
            "LLM output unavailable. Set OPENAI_API_KEY and re-run bun run generate."
      ]
  else
    el "ol" [ HA.class' "marginalia-list" ] (notes <#> renderNote)
  where
  renderNote :: MarginalNote -> Html message
  renderNote note =
    el "li" [ HA.class' "marginalia-item" ]
      [ el "a"
          [ HA.class' "note-ref"
          , HA.href (noteTarget note.readingKind note.lines)
          , HA.createAttribute "data-hl" (noteHighlights note.readingKind note.lines)
          ]
          [ txt (renderLinesLabel lineLabel note.readingKind note.lines) ]
      , el "span" [ HA.class' "note-text" ] [ txt note.text ]
      ]

renderCommentNotes :: forall message. (ReadingKind -> Int -> String) -> ReadingKind -> Array CommentNote -> Html message
renderCommentNotes lineLabel kind notes =
  if Array.length notes == 0 then
    el "div" [ HA.class' "empty-note" ] [ txt "(no notes)" ]
  else
    el "ul" [ HA.class' "comment-list" ] (notes <#> renderOne)
  where
  renderOne :: CommentNote -> Html message
  renderOne note =
    el "li" [ HA.class' "comment-item" ]
      [ el "a"
          [ HA.class' "note-ref"
          , HA.href (noteTarget kind note.lines)
          , HA.createAttribute "data-hl" (noteHighlights kind note.lines)
          ]
          [ txt (renderLinesLabel lineLabel kind note.lines) ]
      , el "span" [ HA.class' "note-text" ] [ txt note.text ]
      ]

renderCommentaryBox :: forall message. (ReadingKind -> Int -> String) -> Boolean -> Artifact -> Html message
renderCommentaryBox lineLabel hasLlm artifact =
  let
    isEmpty =
      Array.length artifact.commentary.reading == 0
        && Array.length artifact.commentary.gospel == 0
        && artifact.commentary.synthesis == ""

    emptyText =
      if hasLlm then
        "(no commentary generated)"
      else
        "LLM output unavailable. Set OPENAI_API_KEY and re-run bun run generate."

    excursusText = String.trim artifact.commentary.excursus
    seminaText = String.trim artifact.commentary.seminaVerbi

    commentaryColumns =
      if isEmpty then
        el "div" [ HA.class' "empty-note" ] [ txt emptyText ]
      else
        el "div" [ HA.class' "commentary-columns" ]
          [ el "section" [ HA.class' "commentary-column" ]
              [ el "div" [ HA.class' "panel-kicker" ] [ txt "On the Reading" ]
              , renderCommentNotes lineLabel firstReadingKind artifact.commentary.reading
              ]
          , el "section" [ HA.class' "commentary-column" ]
              [ el "div" [ HA.class' "panel-kicker" ] [ txt "On the Gospel" ]
              , renderCommentNotes lineLabel gospelKind artifact.commentary.gospel
              ]
          ]

    synthesisNode =
      if artifact.commentary.synthesis == "" then
        []
      else
        [ el "p" [ HA.class' "doctrinal-synthesis" ]
            [ el "span" [ HA.class' "meta-label" ] [ txt "Doctrinal" ]
            , txt (" " <> artifact.commentary.synthesis)
            ]
        ]

    firstSupplement =
      el "section" [ HA.class' "supplement-panel" ]
        [ el "div" [ HA.class' "panel-kicker panel-kicker-strong" ] [ txt "Heterodox Reading" ]
        , el "div" [ HA.class' "supplement-text" ]
            [ txt $
                if excursusText == "" then
                  if hasLlm then "(no heterodox reading generated)" else emptyText
                else
                  excursusText
            ]
        ]

    secondSupplement =
      el "section" [ HA.class' "supplement-panel" ]
        [ el "div" [ HA.class' "panel-kicker panel-kicker-strong" ] [ txt "Semina Verbi" ]
        , el "div" [ HA.class' "supplement-text" ]
            [ txt $
                if seminaText == "" then
                  if hasLlm then "(no semina verbi generated)" else emptyText
                else
                  seminaText
            ]
        ]
  in
    el "section" [ HA.class' "panel commentary-panel" ]
      ([ el "header" [ HA.class' "panel-header" ]
           [ el "div" [ HA.class' "panel-kicker" ] [ txt "Gloss" ]
           , el "h2" [ HA.class' "panel-title" ] [ txt "Commentary" ]
           ]
       , commentaryColumns
       ]
        <> synthesisNode
        <> [ firstSupplement, secondSupplement ])

artifactDocument :: RenderConfig -> Artifact -> Html Unit
artifactDocument config artifact =
  let
    firstReading = Array.find (\r -> r.kind == firstReadingKind) artifact.readings
    gospelReading = Array.find (\r -> r.kind == gospelKind) artifact.readings
    translationLabel = case Array.head artifact.readings of
      Nothing -> "Scripture"
      Just r -> r.translation.name <> " (" <> String.toUpper r.translation.id <> ")"
    translationLink = case Array.head artifact.readings of
      Nothing -> navLink "https://www.gutenberg.org/" translationLabel
      Just r -> navLink (translationHref r.translation.id) translationLabel
    lineLabel = lineLabelForKind artifact.readings
    hasLlm = Array.length artifact.llm.calls > 0
    heroLinks =
      Array.catMaybes
        [ if artifact.source.itemUrl == "" then Nothing else Just (navLink artifact.source.itemUrl "Vatican News")
        , if config.homeHref == "" then Nothing else Just (navLink config.homeHref "Latest")
        , if config.archiveHref == "" then Nothing else Just (navLink config.archiveHref "Archive")
        , if config.permalinkHref == "" then Nothing else Just (navLink config.permalinkHref "Permalink")
        ]

    readingNodes =
      Array.catMaybes
        [ map renderReadingBox firstReading
        , map renderReadingBox gospelReading
        ]
  in
    el "html" [ HA.lang "en" ]
      [ documentHead config.assetPrefix ("Verbum Diei - " <> artifact.date)
      , el "body" [ HA.class' "cathedral-body" ]
          [ el "main" [ HA.class' "cathedral-layout" ]
              [ el "header" [ HA.class' "hero-panel" ]
                  [ el "div" [ HA.class' "hero-title-wrap" ]
                      [ el "div" [ HA.class' "hero-kicker" ] [ txt "Daily Office of the Word" ]
                      , el "h1" [ HA.class' "hero-title" ] [ txt "Verbum Diei" ]
                      , el "div" [ HA.class' "hero-date" ] [ txt artifact.date ]
                      ]
                  , el "nav" [ HA.class' "hero-nav" ] heroLinks
                  ]
              , renderObservances artifact
              , el "aside" [ HA.class' "panel marginalia-panel" ]
                  [ el "header" [ HA.class' "panel-header" ]
                      [ el "div" [ HA.class' "panel-kicker" ] [ txt "Margin" ]
                      , el "h2" [ HA.class' "panel-title" ] [ txt "Marginalia" ]
                      ]
                  , renderMarginalia lineLabel hasLlm artifact.marginalia
                  ]
              , el "section" [ HA.class' "reading-stack" ] readingNodes
              , renderCommentaryBox lineLabel hasLlm artifact
              , el "footer" [ HA.class' "site-footer" ]
                  [ el "p" [ HA.class' "footer-note" ]
                      [ txt "Scripture text: "
                      , translationLink
                      , txt " - public domain. For the official source see Vatican News above."
                      ]
                  , el "p" [ HA.class' "footer-note" ]
                      [ txt "Marginalia and glosses are generated by a language model and offered for reflection, not doctrinal instruction." ]
                  , el "p" [ HA.class' "footer-note" ]
                      [ txt "Source code: "
                      , navLink "https://github.com/brickfrog/verbum-diei" "github.com/brickfrog/verbum-diei"
                      ]
                  ]
              ]
          , leaf "script" [ HA.src (config.assetPrefix <> "app.js"), HA.createAttribute "defer" "defer" ]
          ]
      ]

archiveDocument :: ArchiveConfig -> Array String -> Html Unit
archiveDocument config dates =
  let
    dayLinks =
      dates <#> \d ->
        el "li" [ HA.class' "archive-item" ]
          [ el "a" [ HA.class' "site-link archive-link", HA.href (config.dayHrefPrefix <> d <> "/") ] [ txt d ] ]
    archiveNavLinks =
      if config.homeHref == "" then
        []
      else
        [ navLink config.homeHref "Latest" ]
  in
    el "html" [ HA.lang "en" ]
      [ documentHead config.assetPrefix "Verbum Diei - Archive"
      , el "body" [ HA.class' "cathedral-body archive-body" ]
          [ el "main" [ HA.class' "cathedral-layout archive-layout" ]
              [ el "header" [ HA.class' "hero-panel archive-hero" ]
                  [ el "div" [ HA.class' "hero-title-wrap" ]
                      [ el "div" [ HA.class' "hero-kicker" ] [ txt "All Days" ]
                      , el "h1" [ HA.class' "hero-title" ] [ txt "Archive" ]
                      ]
                  , el "nav" [ HA.class' "hero-nav" ]
                      archiveNavLinks
                  ]
              , el "section" [ HA.class' "panel archive-panel" ]
                  [ el "header" [ HA.class' "panel-header" ]
                      [ el "div" [ HA.class' "panel-kicker" ] [ txt "Chronicle" ]
                      , el "h2" [ HA.class' "panel-title" ] [ txt "Days" ]
                      ]
                  , el "ul" [ HA.class' "archive-list" ] dayLinks
                  ]
              , el "footer" [ HA.class' "site-footer" ]
                  [ el "p" [ HA.class' "footer-note" ] [ txt "Generated daily." ]
                  , el "p" [ HA.class' "footer-note" ]
                      [ txt "Source code: "
                      , navLink "https://github.com/brickfrog/verbum-diei" "github.com/brickfrog/verbum-diei"
                      ]
                  ]
              ]
          ]
      ]

appShellDocument :: AppShellConfig -> Html Unit
appShellDocument config =
  el "html" [ HA.lang "en" ]
    [ documentHead config.assetPrefix config.pageTitle
    , el "body" [ HA.class' "cathedral-body app-shell-body" ]
        [ el "main"
            [ HA.id "app-root"
            , HA.class' "cathedral-layout"
            , HA.createAttribute "data-default-view" config.defaultView
            , HA.createAttribute "data-asset-prefix" config.assetPrefix
            ]
            []
        , leaf "script" [ HA.src (config.assetPrefix <> "app.js"), HA.createAttribute "defer" "defer" ]
        ]
    ]
