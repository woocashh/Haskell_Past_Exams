module Exam where

import Data.Char
import Data.Maybe
  
type Name = String

type Attributes = [(Name, String)]

data XML = Null | Text String | Element Name Attributes [XML]
         deriving (Eq, Show)

type Stack = [XML]

-----------------------------------------------------------------------
-- Some useful show/print functions

-- The 'show' function for XML objects
showXML :: XML -> String
showXML (Text t)
  = t
showXML (Element n as es)
  = "<" ++ n ++ showAtts as ++ ">" ++ concatMap showXML es ++ "</" ++ n ++ ">"
  where
    showAtts as = concatMap showAtt as
    showAtt (n, v) = " " ++ n ++ "=" ++ "\"" ++ v ++ "\""

-- The 'show' function for lists of XML objects
showXMLs :: [XML] -> String
showXMLs
  = concatMap showXML

-- Prints an XML object to the terminal
printXML :: XML -> IO()
printXML 
  = putStrLn . showXML

-- Prints a list of XML objects to the terminal (useful for testing the
-- output from expandXML')
printXMLs :: [XML] -> IO()
printXMLs
  = mapM_ printXML

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- Part I

skipSpace :: String -> String
skipSpace
  = dropWhile isSpace

getAttribute :: String -> XML -> String
getAttribute nm (Element _ nms _)
  = fromMaybe "" (lookup nm nms)
getAttribute _ _
  = ""

getChildren :: String -> XML -> [XML]
getChildren name (Element _ _ children)
  = [chs | chs@(Element nm atr ch) <- children, nm == name]
getChildren _ _
  = []

getChild :: String -> XML -> XML
getChild name xml
  | null children = Text ""
  | otherwise     = head children 
  where
    children = getChildren name xml

addChild :: XML -> XML -> XML
-- Pre: the second argument is an Element
addChild ch (Element nm atrs chs)
  = Element nm atrs (chs ++ [ch])

getValue :: XML -> XML
getValue Null
  = Text ""
getValue (Text t)
  = (Text t)
getValue (Element nm ats chs)
  = foldl adder (Text "") (map getValue chs)
  where
    adder (Text t1) (Text t2)
      = Text (t1 ++ t2)

-------------------------------------------------------------------------
-- Part II

-- Parses an element/attribute name
parseName :: String -> (Name, String)
parseName []
  = error "Error: attempt to read empty name"
parseName s@(c : cs)
  | isAlpha c = break (not . isNameChar) s
  | otherwise = error ("parseName error: name " ++ show s ++
                      " must begin with a letter")
  where
    isNameChar c = isAlpha c || isDigit c || elem c "-."

sentinel :: XML
sentinel 
  = Element "" [] []

addText :: String -> Stack -> Stack
-- Pre: There is at least one Element on the stack
addText txt (xml : xmls)
  = (addChild (Text txt) xml) : xmls

popAndAdd :: Stack -> Stack
-- Pre: There are at least two Elements on the stack
popAndAdd (ch : (xml : xmls))
  = addChild ch xml : xmls

parseAttributes :: String -> (Attributes, String)
-- Pre: The XML attributes string is well-formed
parseAttributes s
  = parseAttributes' [] (skipSpace s)
  where
    parseAttributes' attrs ('>' : rest)
      = (attrs, rest)
    parseAttributes' attrs rest
      = parseAttributes' ((nm, val) : attrs) ((skipSpace.tail) re')
      where
        (nm, re) = parseName rest
        (val, re') = parseVal (tail (dropWhile (/='\"') re)) 

parseVal s
  = span (/='\"') s


parse :: String -> XML
-- Pre: The XML string is well-formed
parse s
  = parse' (skipSpace s) [sentinel]

parse' :: String -> Stack -> XML
parse' "" stc 
  = (\(Element _ _ xmls) -> head xmls) (head stc)
parse' ('<' : ('/' : ss)) stc
  = parse' (tail (dropWhile (/='>') ss)) (popAndAdd stc)
parse' ('<' : ss) stc
  = parse' re' ((Element nm attrs []) : stc)
  where
    (nm, re) = parseName ss
    (attrs, re') = parseAttributes re
parse' ss stc
  = parse' re (popAndAdd ((Text txt) : stc))
  where
    (txt, re) = span (/='<') ss

-------------------------------------------------------------------------
-- Part III

type Context = XML

type XSL = XML

-- Parses XSL and XML source documents and transforms the latter using the
-- former. The output is written to the given file (String).
-- Example use:
--   output "out.html" filmsXSL films
-- To render output.html in a browser, type this at the Linux prompt:
--   firefox output.html &
output :: String -> XML -> XML -> IO()
output file xsl source
  = writeFile file (showXMLs (expandXSL xsl source))

expandXSL :: XSL -> XML -> [XML]
expandXSL xsl source 
  = expandXSL' root xsl
  where
    root = Element "/" [] [source] 

expandXSL' :: Context -> XSL -> [XML]
expandXSL' _ Null
  = [Null]
expandXSL' _ (Text text)
  = [Text text]
expandXSL' context (Element name atts xsls)
  | name == "value-of" && newContext /= []
    = [getValue (head newContext)]
  | name == "value-of" && newContext == []
    = []
  | name == "for-each"
    = concat [expandXSL' newContext' xsl | newContext' <- newContext, xsl <- xsls]
  | otherwise
    = [(Element name atts (concatMap (expandXSL' context) xsls))]
  where
    path = fromJust (lookup "select" atts)
    newContext = (getContext (path ++ "/") context)

getContext :: String -> XML -> [XML]
getContext path xml
  | pathName == ""
    = [xml]
  | not (isElement xml)
    = []
  | head pathName == '@'
    = [Text (getAttribute (tail pathName) xml)]
  | pathName == "."
    = getContext remainingPath xml
  | otherwise
    = concatMap (getContext remainingPath) (getChildren pathName xml)
  where
    pathName = takeWhile (/='/') path
    remainingPath = tail (dropWhile (/='/') path)
    children = getChild pathName xml
    isElement (Element _ _ _) = True
    isElement _ = False
    getName (Element x _ _) = x
    allChildren (Element _ _ x) = x

-------------------------------------------------------------------------
-- Test data for Parts I and II

-- Simple test cases (no whitespace)
s1, s2, s3 :: String
s1
  = "<a>A</a>"
s2 
  = "<a x=\"1\"><b>A</b><b>B</b></a>"
s3
  = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>"

-- Parsed versions of the above
x1, x2, x3 :: XML
x1
  = Element "a" [] [Text "A"]
x2
  = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3
  = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("att","att1")] 
                              [Text "text1"],
                      Element "c" 
                              [("att","att2")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("att","att3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]

casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]

-- Films mark-up of Figure 1
films :: String
films
  = "<filmlist>\n\
    \  <film title = \"Rear Window\">\n\
    \    <director>Alfred Hitchcock</director>\n\
    \    <composer>Franz Waxman</composer>\n\
    \    <year>1954</year>\n\
    \  </film>\n\
    \  <film   title =  \"2001: A Space Odyssey\">\n\
    \    <director>Stanley Kubrick</director>\n\
    \    <composer>Richard Strauss</composer>\n\
    \    <composer>Gyorgy Ligeti</composer>\n\
    \    <composer>Johann Strauss</composer>\n\
    \    <year>1968</year>\n\
    \  </film>\n\
    \  <film title=\"Lawrence of Arabia\"  >\n\
    \    <duration>228</duration>\n\
    \    <director>David Lean</director>\n\
    \    <composer>Maurice Jarre</composer>\n\
    \  </film>\n\
    \</filmlist>\n\n\n"

-- Parsed version of films ('parse films'), suitably formatted
filmsParsed :: XML
filmsParsed
  = Element "filmlist" 
            [] 
            [Text "\n  ",
             Element "film" [("title","Rear Window")]
                            [Text "\n    ",
                             Element "director" [] [Text "Alfred Hitchcock"],
                             Text "\n    ",
                             Element "composer" [] [Text "Franz Waxman"],
                             Text "\n    ",
                             Element "year" [] [Text "1954"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","2001: A Space Odyssey")] 
                            [Text "\n    ",
                             Element "director" [] [Text "Stanley Kubrick"],
                             Text "\n    ",
                             Element "composer" [] [Text "Richard Strauss"],
                             Text "\n    ",
                             Element "composer" [] [Text "Gyorgy Ligeti"],
                             Text "\n    ",
                             Element "composer" [] [Text "Johann Strauss"],
                             Text "\n    ",
                             Element "year" [] [Text "1968"],
                             Text "\n  "],
             Text "\n  ",
             Element "film" [("title","Lawrence of Arabia")] 
                            [Text "\n    ",
                             Element "duration" [] [Text "228"],
                             Text "\n    ",
                             Element "director" [] [Text "David Lean"],
                             Text "\n    ",
                             Element "composer" [] [Text "Maurice Jarre"],
                             Text "\n  "],
             Text "\n"]

-------------------------------------------------------------------------
-- XSL tests

-- value-of test cases
xsl1, xsl2, xsl3, xsl4, xsl5, xsl6, xsl7, 
  xsl8, xsl9 :: String
xsl1
  = "<value-of select = \"a/b/c\"></value-of>"
xsl2
  = "<value-of select = \"a/b\"></value-of>"
xsl3
  = "<value-of select = \"a/b/d\"></value-of>"
xsl4
  = "<value-of select = \"a/b/c/@att\"></value-of>"
xsl5
  = "<value-of select = \"./a/./b/c/./.\"></value-of>"
xsl6
  = "<t1><t2>Preamble</t2><t3><value-of select = \"a/b/c\"></value-of></t3></t1>"

-- for-each test cases
xsl7
  = "<for-each select=\"a/b/c\"><value-of select=\"./@att\"></value-of>\
    \</for-each>" 
xsl8
  = "<for-each select=\"a/b\"><t><value-of select=\"c\"></value-of></t>\
    \</for-each>" 
xsl9
  = "<for-each select=\"a/b\"><t1><value-of select=\"absent\"></value-of>\
    \</t1></for-each>"
        
-- Parsed versions of the above
xsl1Parsed, xsl2Parsed, xsl3Parsed, xsl4Parsed, xsl5Parsed,
  xsl6Parsed, xsl7Parsed, xsl8Parsed, xsl9Parsed :: XML
xsl1Parsed
  = Element "value-of" [("select","a/b/c")] []
xsl2Parsed
  = Element "value-of" [("select","a/b")] []
xsl3Parsed
  = Element "value-of" [("select","a/b/d")] []
xsl4Parsed
  = Element "value-of" [("select","a/b/c/@att")] []
xsl5Parsed
  = Element "value-of" [("select","./a/./b/c/./.")] []
xsl6Parsed
  = Element "t1" 
            [] 
            [Element "t2" [] [Text "Preamble"],
             Element "t3" [] [Element "value-of" [("select","a/b/c")] []]]

xsl7Parsed
  = Element "for-each" 
            [("select","a/b/c")] 
            [Element "value-of" [("select","./@att")] []]
xsl8Parsed
  = Element "for-each" 
            [("select","a/b")] 
            [Element "t" [] [Element "value-of" [("select","c")] []]]
xsl9Parsed
  = Element "for-each" 
            [("select","a/b")] 
            [Element "t1" [] [Element "value-of" [("select","absent")] []]]

-- XSL template for building a films summary (example from spec.)
filmsXSL :: String
filmsXSL
  = "<html>\n\
    \<body>\n\
    \  <h2>Film List</h2>\n\
    \  <table border=\"1\">\n\
    \    <tr>\n\
    \      <th align=\"left\">Title</th>\n\
    \      <th align=\"left\">Director</th>\n\
    \      <th align=\"left\">Principal composer</th>\n\
    \    </tr>\n\
    \    <for-each select=\"filmlist/film\">\n\
    \      <tr>\n\
    \        <td><value-of select=\"@title\"></value-of></td>\n\
    \        <td><value-of select=\"director\"></value-of></td>\n\
    \        <td><value-of select=\"composer\"></value-of></td>\n\
    \      </tr>\n\
    \    </for-each>\n\
    \  </table>\n\
    \</body>\n\
    \</html>"

-- XSL template for building a list of composers (example from spec.)
composersXSL :: String
composersXSL
  = "<for-each select=\"filmlist/film\">\
      \<h2><value-of select=\"@title\"></value-of> composers</h2>\
      \<ul>\
      \<for-each select=\"composer\">\
        \<li><value-of select=\".\"></value-of></li>\
      \</for-each>\
      \</ul>\
    \</for-each>"

{-
prob :: DataSet -> Attribute -> AttValue -> Double
prob dataSet att attVal
  | not (elem att (fst dataSet)) || length (snd dataSet) == 0
    = 0.0
  | otherwise 
    = (fromIntegral (lookUp attVal (buildFrequencyTable att dataSet))) / 
      (fromIntegral (length (snd dataSet)))
      -}