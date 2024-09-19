{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module GuruPages.HtmlPage where

import Data.Text as T hiding (foldl', map, unwords)
import GuruPages.Model
import NeatInterpolation
import Optics
import Relude
import Text.Blaze
import Text.Blaze.Html5 as B hiding (map)
import Text.Blaze.Html5.Attributes as BA hiding (id, span)

-- PPRD stands for HTMLPreProcessorReplacerDirective
type PPRD :: Type
data PPRD = PPRD
  { needle :: Text, -- the item to search and replace for in the HTML (e.g. <a)
    replacement :: Maybe Text, -- the desired result <a id="1"
    classList :: [Text] -- the desired CSS classes to apply to element eg ["font-bold", "text-cyan-500"],
  }
  deriving (Generic)

makeFieldLabelsNoPrefix ''PPRD

type TopContents :: Type
newtype TopContents = TopContents {value :: Html} deriving (Generic)

makeFieldLabelsNoPrefix ''TopContents

type MainContents :: Type
newtype MainContents = MainContents {value :: Html} deriving (Generic)

makeFieldLabelsNoPrefix ''MainContents

type FooterContents :: Type
newtype FooterContents = FooterContents {value :: Html} deriving (Generic)

makeFieldLabelsNoPrefix ''FooterContents

tailwindPreProcessor :: Text -> Text
tailwindPreProcessor = doWithClassReplacement tailwindApplicationReplacers

doWithClassReplacement :: [PPRD] -> Text -> Text
doWithClassReplacement xs t = preProcessingResults
  where
    replacers = map mkReplacer xs
    uberReplacer = foldl' (.) id replacers
    preProcessingResults = uberReplacer t

mkReplacer :: PPRD -> (Text -> Text)
mkReplacer x =
  replace
    (x ^. #needle)
    (maybeReplacementOrNeedle (x ^. #needle) (x ^. #replacement) (x ^. #classList))

addClasses :: [Text] -> Text -> Text
addClasses [] x = x
addClasses classList x = x <> " class=\"" <> unwords classList <> "\""

maybeReplacementOrNeedle :: Text -> Maybe Text -> [Text] -> Text
maybeReplacementOrNeedle needle replacement classList = maybe (addClasses classList needle) (addClasses classList) replacement

analytics :: Html
analytics = do
  script ! BA.async "" ! src "https://www.googletagmanager.com/gtag/js?id=G-K31CQBL15V" $ ""
  script
    . fromString
    . unpack
    $ [untrimming|
         window.dataLayer = window.dataLayer || [];
         function gtag(){dataLayer.push(arguments);}
         gtag('js', new Date());
         gtag('config', 'G-K31CQBL15V');
      |]

wrapInNiceHtml :: GuruPage -> Html
wrapInNiceHtml page = html $ do
  hHead (page ^. #pageTitle)
  analytics
  hBody
    (hTopSection (page ^. #pageTitle))
    (MainContents . preEscapedText . tailwindPreProcessor $ (page ^. #htmlContents))
    (hFooterSection (page ^. #authors) (page ^. #pageDate))

hHead :: Text -> Html
hHead docTitle' = B.head $ do
  meta ! name "viewport" ! content "width=device-width, initial-scale=1"
  meta ! charset "UTF-8"
  B.title . fromString . unpack $ docTitle'
  link ! rel "stylesheet" ! href "/resources/css/code-highlight-pandoc.css"
  link ! rel "stylesheet" ! href "/resources/css/fonts.css" ! media "print" ! onload "this.media='all'"
  B.style
    . fromString
    . unpack
    $ [untrimming|
    html {
      min-height: 100%;
    }
  |]
  script ! src "https://cdn.tailwindcss.com" $ ""
  script ! src "/resources/js/tailwind.config.js" $ ""
  script
    . fromString
    . unpack
    $ [untrimming|
        (function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
        new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
        j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
        'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
        })(window,document,'script','dataLayer','GTM-W44GSDSJ');
      |]

hBody :: TopContents -> MainContents -> FooterContents -> Html
hBody topSectionContents centerContents footerContents = mBody $ do
  noscript
    $ iframe
    ! src "https://www.googletagmanager.com/ns.html?id=GTM-W44GSDSJ"
    ! height "0"
    ! width "0"
    ! BA.style "display:none;visibility:hidden"
    $ ""

  B.div ! class_ "flex flex-wrap flex-col gap-4 items-center justify-center" $ do
    topSectionContents ^. #value

  B.div
    ! class_ "flex flex-col items-center justify-center text-left"
    $ B.div
    ! class_ "my-6 w-full md:w-3/4 p-4"
    $ do
      centerContents ^. #value

  footer ! class_ "flex flex-wrap flex-col gap-4 items-center justify-center text-center" $ do
    B.div ! class_ "my-6 w-full p-4" $ footerContents ^. #value

hTopSection :: Text -> TopContents
hTopSection pageTitle = TopContents $ do
  h1 ! class_ "text-2xl font-bold text-center my-2 font-serif" $ fromString . unpack $ pageTitle
  nav $ do
    B.div ! class_ "flex gap-4 flex-wrap" $ do
      a ! class_ "font-bold text-cyan-500" ! href "/index.html" $ "Home"
      a ! class_ "font-bold text-cyan-500" ! href "/blog/index.html" $ "Blog"
      a ! class_ "font-bold text-cyan-500" ! href "/cv/index.html" $ "CV/Resume"
      a ! class_ "font-bold text-cyan-500" ! href "https://gitlab.com/jjba-projects" $ "GitLab"
      a ! class_ "font-bold text-cyan-500" ! href "https://www.linkedin.com/in/josepjba/" $ "LinkedIn"
      a ! class_ "font-bold text-cyan-500" ! href "https://www.wikimusic.jointhefreeworld.org/" $ "WikiMusic"
      a ! class_ "font-bold text-cyan-500" ! href "https://www.casadelcata.es/" $ "Holiday in the Canary Islands ?"

hFooterSection :: [Text] -> Maybe Text -> FooterContents
hFooterSection authors maybeDate = FooterContents $ do
  p ! class_ "text-sm" $ preEscapedText "Copyright &copy; 2024 Josep Jesus Bigorra Algaba (JJBA)"
  p ! class_ "text-sm mt-2" $ do
    B.span ! class_ "text-sm" $ "All source code is licensed and available under the "
    a ! class_ "text-sm text-cyan-500 font-bold" ! href "https://www.gnu.org/licenses/gpl-3.0.en.html" $ "GNU GPL v3 license or newer"

  p ! class_ "mt-2 mb-6 text-sm " $ "All media content is available under the Creative Commons License 4.0 or newer"
  (p ! class_ "mt-2 text-sm") . fromString . unpack $ "This page was written by: " <> T.intercalate ", " authors
  p ! class_ "mt-2 mb-6 text-sm " $ do
    B.span ! class_ "text-sm" $ "Find the source code powering this page: "
    a ! class_ "text-sm text-cyan-500 font-bold" ! href "https://gitlab.com/jjba-projects/jjba" $ "on my GitLab"

  maybe "" dateToHtml maybeDate

  p ! class_ "mt-2" $ do
    B.span "Go back to "
    a
      ! class_ "text-cyan-500 font-bold"
      ! href "#"
      ! onclick "window.scroll({top: 0, left: 0, behavior: 'smooth'})"
      $ "the top of the page"
  p
    ! class_ "mt-4 text-sm text-gray-500"
    $ "Contact jointhefreeworld's administrator, Joe (jjbigorra@gmail.com)"
  where
    dateToHtml date = (p ! class_ "mt-2") . fromString . unpack $ "Page date: " <> date

mBody :: Html -> Html
mBody = body ! class_ "p-3" ! BA.style ("background-image: " <> bodyGradient)

bodyGradient :: AttributeValue
bodyGradient = "radial-gradient(at 0% 100%, rgba(6, 114, 229, 0.33) 0px, rgba(0, 0, 0, 0) 50%), radial-gradient(at 98% 100%, rgba(143, 15, 34, 0.33) 0px, rgba(0, 0, 0, 0) 50%)"

tailwindApplicationReplacers :: [PPRD]
tailwindApplicationReplacers =
  [ PPRD
      { needle = "<a",
        replacement = Nothing,
        classList = ["font-bold", "text-cyan-500", "hover:text-cyan-700"]
      },
    PPRD
      { needle = "<p",
        replacement = Nothing,
        classList = ["my-4", "text-lg"]
      },
    PPRD
      { needle = "<h1",
        replacement = Nothing,
        classList = ["text-2xl", "font-bold", "font-serif", "my-6"]
      },
    PPRD
      { needle = "<h2",
        replacement = Nothing,
        classList = ["text-xl", "font-bold", "font-serif", "my-6", "text-emerald-700"]
      },
    PPRD
      { needle = "<h3",
        replacement = Nothing,
        classList = ["text-lg", "font-bold", "font-serif", "my-4"]
      },
    PPRD
      { needle = "<h4",
        replacement = Nothing,
        classList = ["text-md", "font-bold", "font-serif", "my-4"]
      },
    PPRD
      { needle = "<h5",
        replacement = Nothing,
        classList = ["text-md", "font-bold", "font-serif", "my-4"]
      },
    PPRD
      { needle = "<img",
        replacement = Nothing,
        classList = ["rounded-xl"]
      },
    PPRD
      { needle = "<ul",
        replacement = Nothing,
        classList = ["list-disc", "ml-6"]
      },
    PPRD
      { needle = "<blockquote",
        replacement = Nothing,
        classList = ["text-lg", "italic", "font-semibold", "p-4", "rounded-xl", "text-gray-600", "bg-slate-300/[.20]"]
      }
  ]
