{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE TypeFamilies #-}

-- to generate the poster:
-- ghc --make poster.hs
-- ./poster -o poster.pdf -w 1685 or
-- ./poster -o poster.png -w 10000    (to generate a png with width 10000)
-- to understand the code, go to http://projects.haskell.org/diagrams/manual/diagrams-manual.html

-- expected format: Din A1: 594 x  841
-- or               Din A0: 841 x 1189 mm  (sqrt 2 / 1)

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
-- import Diagrams.Backend.SVG.CmdLine
import Data.Word
import Graphics.SVGFonts
import qualified Diagrams.TwoD.Size as Size
import Text.Highlighting.Kate
import Data.Tree
import Data.Colour hiding (atop)
import Data.Maybe
import Paths_unixPoster(getDataFileName)

main = do folderImg <- getDataFileName "img/folder.png"
          filesImg <- getDataFileName "img/files.png"
          pdp7 <- getDataFileName "img/pdp7_3.png"
          let images = [folderImg,filesImg,pdp7]
          mainWith (unixPoster images :: Diagram B)

--------------------------------------------------
-- basic building blocks of the diagram
--------------------------------------------------

unixPoster images
           = {-# SCC "poster" #-} strutY 3 ===
             header # centerXY
             === strutY 4 ===
             poster_portrait images # centerXY -- poster_body
             # applyAll (map linesToLProcessing
                      ["wc","cut","grep","head","tail","tee","more","less","sort","uniq","tr","rev"]) # centerXY
             # applyAll (map toLines ["find","tree","ls","cat"]) # centerXY
             # applyAll (map toSeveralFilesInput ["cmp","diff"]) # centerXY
             # applyAll (map folderToChild ["ls","mkdir","rmdir","cd","tree","pwd","cp","mv","find"]) # centerXY
             # applyAll (map filesToChild ["ls","cp","mv","chmod","cat","touch","rm","ln","stat"]) # centerXY
             # applyAll (map pipingExample [("pwd", "piping7"), ("piping7", "rev"), ("piping7", "cut"),
                                            ("ls", "piping0"), ("piping0", "wc"),
                                            ("piping1", "ps"), ("piping1","head"),
                                            ("piping2", "ps"), ("piping2", "wc"),
                                            ("piping3", "ps"), ("piping3", "grep"),
                                            ("piping4", "history"), ("piping4", "grep"),
                                            ("piping5", "ps"), ("piping5", "less"),
                                            ("piping6", "history"), ("piping6", "grep"), ("piping6","cut"),
                                            ("piping8", "ls"), ("piping8", "grep"),
                                            ("redir1",  "ps"),
                                            ("redir2",  "date"),
                                            ("find", "redir3"),-- ("redir3", "outputRedirection"),
                                            ("find", "redir4"),-- ("redir4", "outputRedirection"),
                                            ("grep", "redir5"),("redir5", "outputRedirection"),
                                            ("find", "redir6")-- ,("redir6", "outputRedirection")
                                         ])
             ===
             scriptingH # centerXY -- scriptingV
             ===
             strutY 2
             ===
             copyright # centerXY
             ===
             strutY 2

-- connections = {-# SCC "connections" #-}

poster_body [folder,fil,pdp7] = ((input folder fil) ||| strutX 3 ||| commandBodyL) |||
                (  (outp ||| strutX 5 ||| commandBodyM ||| redirection ||| commandBodyR ||| strutX 3)
                   ===
                   (scriptingV ||| strutX 3 ||| dollarOps ||| strutX 3 ||| (files pdp7))
                )

poster_portrait [folder,fil,pdp7] = ((input folder fil) ||| strutX 3 ||| ( (commandBodyL ||| (outp
                                                               === strutY 2 ===
                                                               redirection))
                                            ===
                                            ((files pdp7) ||| strutX 3 ||| dollarOps) )
                  )
                  ||| strutX 5 |||
                  (commandBodyM === strutY 5 === commandBodyR)


folderToChild child
  = withName "folder" $ \rb ->
    withName child    $ \cb ->
 ( flip atop (   fromMaybe origin (traceP (location rb) unit_X rb)
             ~~~ fromMaybe origin (traceP (location cb) unitX cb) # lc salmon) )

filesToChild child
  = withName "files" $ \rb ->
    withName child   $ \cb ->
  flip atop (   fromMaybe (location rb) (traceP (location rb) unit_X rb)
            ~~~ fromMaybe (location cb) (traceP (location cb) unitX cb) # lc seagreen)

toSeveralFilesInput child
  = withName "twoFiles" $ \rb ->
    withName child $ \cb ->
  flip atop (   fromMaybe origin (traceP (location rb) unit_X rb)
            ~~~ fromMaybe origin (traceP (location cb) unitX cb) # lc silver # opacity 0.8)


linesToLProcessing child
  = withName "lines" $ \rb ->
    withName child   $ \cb ->
  flip atop (   fromMaybe origin (traceP (location rb) unit_X rb)
            ~~~ fromMaybe origin (traceP (location cb) unitX cb) # lc plum # opacity 0.2)

toLines child
  = withName child   $ \rb ->
    withName "lines" $ \cb ->
  flip atop (   fromMaybe origin (traceP (location rb) unit_X rb)
            ~~~ fromMaybe origin (traceP (location cb) unitX cb) # lc plum # opacity 0.2)

pipingExamples (box0,boxes) = mconcat $ map (pipingExample box0) boxes

pipingExample (from, to)
  = withName from $ \rb ->
    withName to $ \cb ->
  flip atop (   fromMaybe origin (traceP (location rb) unit_X rb)
            ~~~ fromMaybe origin (traceP (location cb) unitX cb) # lc silver)

-- connect two points with a bezier curve horizontally
p1 ~~~ p2 = (stroke $ translate (p1 .-. origin) $ pathFromTrail segs) # lwL 0.4
  where c1 = p1 .+^ (r2 (3,0))
        c2 = p2 .+^ (r2 (-3,0))
        segs = fromSegments [bezier3 (c1 .-. p1) (c2 .-. p1) (p2 .-. p1)]


header = (textLin "UNIX - Commands" 10 black) # alignBR

input folderImg filesImg = (strutY 15
         ===
         i # centerXY
         === strutY 35 ===
         folder # centerXY
         ===
         folderPic folderImg # named "folder"
         === strutY 70 ===
         files # centerXY
         ===
         filesPic filesImg # named "files"
         === strutY 33 ===
         h # centerXY 
         === strutY 2 ===
         twoFilesIcon # named "twoFiles" # centerXY
         ) # alignTL
  where
    i = textLin "Input" 4 black # alignBR
    folder = textLin "Directories" 3 black # alignBR
    files = textLin "Files" 3 black # alignBR
    h = textLin "Two Files" 3 black # alignBR

folderPic f = mempty -- image f 10 10 # centerXY
inputRedirection = textBox ["InputRedirection"] 2 white red 0.3 # centerXY
filesPic f = mempty -- image f 10 10 # centerXY

linesIcon = textBox ["------","------","------","------"] 1 white blue 0.3
twoFilesIcon = linesIcon ||| strutX 1 ||| linesIcon

commandBodyL = fileSystem # alignTL

fileSystem = {-# SCC "fileSystem" #-} groupedCommands "File System" 120 230
     [unix_cd , unix_pwd, unix_mkdir, unix_rmdir, unix_find ||| strutX 3 ||| ( redir_Example3 # centerXY # named "redir3"
                                                            === strutY 1 ===
                                                            redir_Example4 # centerXY # named "redir4"
                                                            === strutY 1 ===
                                                            redir_Example6 # centerXY # named "redir6"
                                                          ) # alignTL,
      unix_tree, unix_ls, unix_cp, unix_mv, unix_cat, unix_echo,
      (unix_chmod ||| unix_permissions),
      unix_touch, unix_rm, unix_ln, unix_stat, unix_cmp, unix_diff, unix_df, unix_du -- , unix_quota # named "quota"
     ]

outp = ( i # centerXY
           === strutY 2 ===
           piping # centerXY # named "piping"
           === strutY 4 ===
           piping_Example7 # centerXY # named "piping7"
           === strutY 4 ===
           piping_Example0 # centerXY # named "piping0"
           === strutY 4 ===
           piping_Example2 # centerXY # named "piping2"
           === strutY 4 ===
           piping_Example3 # centerXY # named "piping3"
           === strutY 4 ===
           piping_Example1 # centerXY # named "piping1"
           === strutY 4 ===
           g # centerXY
           === strutY 2 ===
           linesIcon # centerXY # named "lines"
           === strutY 5 ===
           piping_Example4 # centerXY # named "piping4"
           === strutY 5 ===
           piping_Example5 # centerXY # named "piping5"
           === strutY 5 ===
           piping_Example6 # centerXY # named "piping6"
         ) # alignTL
  where
    i = textLin "Output/Input" 4 black # alignBR
    g = textLin "File/Lines" 3 black # alignBR


commandBodyM = lineProcessing # alignTL

lineProcessing = groupedCommands "Processing of Lines" 95 160
      [unix_rev, unix_wc, unix_cut, unix_grep, unix_tee,
       unix_head ||| strutX 1 ||| unix_tail, 
       unix_more, unix_less, unix_sort, unix_uniq, unix_tr
      ]

redirection = (
         strutY 10
         ===
         i # centerXY
         === strutY 2 ===
         redir # centerXY # named "outputRedirection"
         === strutY 5 ===
         redir_Example0 # centerXY # named "redir0"
         === strutY 5 ===
         redir_Example1 # centerXY # named "redir1"
         === strutY 5 ===
         redir_Example2 # centerXY # named "redir2"
         === strutY 5 ===
         inpRedirection_Example # centerXY
         ) # alignTL
 where i = textLin "Output Redirection" 4 black # alignBR

commandBodyR = processes # alignTL

processes = groupedCommands "Processes and OS" 95 150
      [unix_ps,
       unix_top,unix_and,unix_jobs,unix_bg,unix_fg,unix_fork,unix_exec,unix_exit,unix_wait,
       unix_kill,unix_killall,unix_sudo,unix_su,unix_finger,unix_who,unix_which,
       unix_env ||| strutX 1 ||| unix_man ||| strutX 1 ||| unix_info,
       unix_date ||| strutX 1 ||| unix_cal    ||| strutX 1 ||| unix_free ||| strutX 1 ||| unix_uptime ||| strutX 1 ||| unix_clear,
       unix__exit ||| unix_sleep, unix_history]

copyright = -- (textBit "\x00a9" 6 black # alignBR) ||| strutX 2 ||| 
            (textLin "Generated with the Haskell diagrams library" 4 black)


scriptingV = (header === strutY 1 === ((scripting0 
                                        ===
                                        strutY 1
                                        ===
                                        scripting1
                                        ===
                                        strutY 1
                                        ===
                                        scripting2
                                        ===
                                        strutY 1
                                        ===
                                        scripting3) # centerXY ) ) # alignTL
  where
    header = textLin "Scripting" 5 black # centerXY

scriptingH = (header === strutY 3 === (scripting0
                                       ||| strutX 1 |||
                                       scripting1
                                       ||| strutX 1 |||
                                       (scripting2
                                        ===
                                        strutY 1
                                        ===
                                        scripting3) ) # centerXY )
  where
    header = textLin "Scripting" 5 black # centerXY


files i = strutX 2 ||| (header === strutY 3 === (((drawTreeDiagram (fileSystemTree2 blue)) # centerXY) `atop` im)) # alignTL
  where
    header = textLin "Files and Directories" 5 black # centerXY
    im = mempty -- image i 90 70 # centerXY


redir = textBoxWithHeader
  "Output Redirection"
  ["of the channels",
   "stdout = 1,",
   "stderr = 2:",
   ">     Redirect stdout",
   "2>    Redirect stderr",
   "2>&1  Redirect stderr",
   "      to stdout",
   ">>    Append to stdout",
   "2>&1 | Pipe stdout and",
   "       stderr to",
   "       another command" ] 2 white indianred # alignTL

redir_Example0 = textBoxWithHeader
  "echo \"Obama\" > president"
  ["writes to file 'president'",
   "instead of the screen"
  ] 1.75 white indianred # alignTL

redir_Example1 = textBoxWithHeader
  "ps -ef > procList"
  ["the table showing all",
   "processes is output to",
   "file 'procList'"
  ] 2 white indianred # alignTL

redir_Example2 = textBoxWithHeader
  "date >> procList"
  ["current date and time",
   "is appended at the end",
   "of file 'procList'"
  ] 2 white indianred # alignTL

redir_Example3 = textBoxWithHeader
  "find peanuts 2> badReport"
  ["the errors - if any - resulting",
   "from searching for peanuts are",
   "output to file 'badReport'",
   "instead of the screen",
   "at the end of file 'procList'"
  ] 2 white indianred # alignTL

redir_Example4 = textBoxWithHeader
  "find peanuts 1> result 2> badReport"
  ["the output from searching for",
   "peanuts are written into file",
   "'result' and the errors - if any",
   " - to file 'badReport' instead ",
   "of the screen"
  ] 2 white indianred # alignTL

redir_Example5 = textBoxWithHeader
  "grep 0.5 integers 2> /dev/null"
  ["sends the error report to /dev/null",
   "- the system's \"black hole\" -",
   "instead of printing it to the screen"
  ] 2 white indianred # alignTL

redir_Example6 = textBoxWithHeader
  "find peanuts 2>> failures 1>> found"
  ["the output from searching for",
   "peanuts are appended to file 'found'",
   "and the errors - if any - to file",
   "'failures' instead of being printed",
   "to the screen"
  ] 2 white indianred # alignTL



inpRedirection_Example = textBoxWithHeader
  "Input Redirection"
  [ "while read x; do",
    "echo -e $(wc -w $x)",
    "done < fileToBeRead",
    "the content of file",
    "'fileToBeRead' is used",
    "as input for 'read x',",
    "thus allowing to process",
    "'fileToBeRead' line-by-line"
  ] 2 white indianred

piping = textBoxWithHeader
  "Piping"
  ["The output of","one command",
   "becomes the input","of the other"] 2 white grey # alignTL

piping_Example0 = textBoxWithHeader
  "ls | wc -w"
  ["counts the files/directories",
   "contained in the current",
   "directory"
  ] 2 white grey # alignTL

piping_Example1 = textBoxWithHeader
  "ps -e | head"
  ["displays the first 10 lines", 
   "of the table that shows all",
   "processes"
  ] 2 white grey # alignTL

piping_Example2 = textBoxWithHeader
  "ps -e | wc -l"
  ["counts the number of rows in",
   "the table that shows all",
   "processes"
  ] 2 white grey # alignTL

piping_Example3 = textBoxWithHeader
  "ps -e | grep bash"
  ["shows all processes that",
   "are shells"
  ] 2 white grey # alignTL

piping_Example4 = textBoxWithHeader
  "history | grep \"|\""
  ["shows all commands in the",
   "history that involved piping"
  ] 2 white grey # alignTL

piping_Example5 = textBoxWithHeader
  "ps -e | less"
  ["displays the table that shows", 
   "all processes in a scrollable",
   "view"
  ] 2 white grey # alignTL

piping_Example6 = textBoxWithHeader
  "history | grep \"|\" | cut -d \"|\" -f2"
  [ "shows all commands",
   "behind a (first) pipe"
  ] 1.5 white grey # alignTL

piping_Example7 = textBoxWithHeader
  "pwd | rev | cut -d \"/\" -f1 | rev"
  ["displays the name of the current",
   "directory without the leading path"
  ] 1.5 white grey # alignTL

piping_Example8 = textBoxWithHeader
  "ls -l | grep -v ^-"
  ["displays all contents of the current",
   "directory that are not regular files"
  ] 2 white # fc grey # alignTL


--------------------------------------------------
-- helper functions for the layout
--------------------------------------------------
--

groupedCommands txt w h commands = (((header # centerXY === strutY 5 === commandRows # centerXY) # alignTL) `atop` roundedBox)
                                   # centerXY # pad 1.05 # alignBL # lc black # fc black
  -- `atop` roundedBox
  where
    -- placing commands below each other
    header = textLin txt 5 black # alignBL
    commandRows = vcat' (with & sep .~ 1) commands # alignTL
    roundedBox = (roundedRect width hhh 2) # alignTL # fc grey # lc grey # opacity 0.2
    width = w -- Size.width  (placedCommands :: D R2)
    hhh = h -- Size.height (placedCommands :: D R2)

command t = textBox [t] 2 black red 0.3 # centerXY # named t # alignTL 

description t | null t = mempty
              | otherwise = textBox2 t 2 black blue # alignTL

c_option op comment | null op = mempty
                    | (length op) + (length comment) > 100 = (textBox  [op]      1.5 black green 0.3)
                                                              ===
                                                             (textBox2 [comment] 1.7 black white)
                    | otherwise = (textBox  [op]      1.5 black green 0.3)
                                  |||
                                  (textBox2 [comment] 1.7 black white)

-- command and options aligned from left to right
                 --   command    description    [ (Option,  Example output from this option) ]
commandWOptions c d ops = commandBox c d (map optAndOutput ops)

-- command and for every option a new line
commandVerticalOptions c d ops = commandVBox c d (map vOptAndOutput ops)

commandDescriptionExample c d lines = c ||| d ||| (terminal lines 1.5)

-- commandBox :: Diagram b -> Diagram b -> [Diagram b] -> Diagram b
commandBox c d opts = (c ||| d) === placedOptions # fc red # lc red
  where
    placedOptions = hcat' (with & sep .~ 0.5) opts

-- commandVBox :: V b ~ V2 => Diagram b -> Diagram b -> [Diagram b] -> Diagram b
commandVBox c d opts = (c ||| d) === placedOptions # fc red # lc red
  where
    placedOptions = vcat' (with & sep .~ 0.5) opts

-- textbox with example code below the option
optAndOutput (opt,lines) | (length lines) <= 3 = strutX 1 ||| (opt === (terminal lines 1.5)) # centerXY # pad 1.02 # alignTL
                         | otherwise           = strutX 1 ||| (opt === (terminal lines 1)) # centerXY # pad 1.02 # alignTL

vOptAndOutput (opt,lines) | (length lines) <= 3 = strutX 1 ||| (opt ||| ((terminal lines 1.5) # alignTL ) ) # alignTL
                          | otherwise           = strutX 1 ||| (opt ||| ((terminal lines 1) # alignTL ) ) # alignTL

fancyRoundedBox w h hh c = ( (roundedRect (w-0.4-size*0.04) (hh-0.4-size*0.04) (h/5)) # centerXY # lwL 0 # fc (blend 0.5 c white)
                           `atop`
                             (roundedRect w hh (h/5))          # centerXY # lwL 0 # fc (blend 0.5 c black) ) # alignTL # opacity 0.7
  where size | w > hh    = hh
             | otherwise = w

-- reduce the size of an object by len in all directions
border hrel w h obj | w > len && h > len = obj # scaleX ((w-len)/w)
                                               # scaleY ((h-len)/h)
                                               # translateX (len/2)
                                               # translateY (-len/2)
                    | otherwise           = obj
  where len = hrel*h

terminal lines h = (border 0.3 w hh placedTextLines) `atop` roundedBox
  where
    -- placing textLines below each other
    placedTextLines = vcat' (with & sep .~ h/5) textLines # alignTL # lc white # fc white
    textLines = map (\t -> (textBit t h white) # alignTL) lines
    roundedBox = (roundedRect w hh 0.5) # alignTL # lc black # fc black -- # opacity 0.5
    w = Size.width  (placedTextLines :: D V2 Double)
    hh = Size.height (placedTextLines :: D V2 Double)

textBox lines h c0 c1 b = (border b w hh placedTextLines) `atop` roundedBox
  where
    -- placing textLines below each other
    placedTextLines = vcat' (with & sep .~ h/5) textLines # alignTL
    textLines = map (\t -> (textBit t h c0) # alignTL) lines
    roundedBox = fancyRoundedBox w h hh c1
    w = Size.width  (placedTextLines :: D V2 Double)
    hh = Size.height (placedTextLines :: D V2 Double)

textBox2 lines h c0 c1 = (border 0.3 w hh placedTextLines) `atop` roundedBox
  where
    -- placing textLines below each other
    placedTextLines = vcat' (with & sep .~ h/5) textLines # alignTL
    textLines = map (\t -> (textLin t h c0) # alignTL) lines
    roundedBox = fancyRoundedBox w h hh c1
    w = Size.width  (placedTextLines :: D V2 Double)
    hh = Size.height (placedTextLines :: D V2 Double)


textBoxWithHeader line lines h c0 c1 = (border 0.3 w hh placedTextLines) `atop` roundedBox
  where
    -- placing textLines below each other
    placedTextLines = ((textBit line h black # centerXY) === (vcat' (with & sep .~ h/5) textLines # centerXY)) # alignTL
    textLines = map (\t -> (textBit t h c0) # alignTL) lines
    roundedBox = fancyRoundedBox w h hh c1
    w = Size.width  (placedTextLines :: D V2 Double)
    hh = Size.height (placedTextLines :: D V2 Double)


highlightedText code h c lang = (border 0.15 w hh placedTextLines) `atop` roundedBox
  where
    -- placing textLines below each other
    placedTextLines = vcat' (with & sep .~ h/5) textLines # alignTL
    textLines = map l hlines # alignTL
    l line = hcat' (with & sep .~ 0.1) (map token line)
    token (CommentTok,str)  = textBit str h blue # fc blue # alignBR
    token (KeywordTok,str)  = textBit str h red # fc blue # alignBR
    token (FloatTok,str)    = textBit str h orange # fc blue # alignBR
    token (CharTok,str)     = textBit str h violet # fc blue # alignBR
    token (StringTok,str)   = textBit str h violet # fc blue # alignBR
    token (FunctionTok,str) = textBit str h black # fc blue # alignBR
    token (_,str)            = textBit str h grey # fc blue # alignBR
    roundedBox = (roundedRect w hh (h/5)) # alignTL # opacity 0.3
    w = Size.width  (placedTextLines :: D V2 Double)
    hh = Size.height (placedTextLines :: D V2 Double)
    hlines :: [SourceLine]
    hlines = map concat $ map (highlightAs lang) code

textBit t h c | null t = mempty
              | otherwise = {-# SCC "textBit" #-} textSVG_ (TextOpts bit INSIDE_H HADV False 1 h) t # fc c # lc c
                                                                                                    # lwL 0.1 # fillRule EvenOdd

textLin t h c | null t = mempty
              | otherwise = {-# SCC "textLin" #-} textSVG_ (TextOpts lin INSIDE_H HADV False 1 h) t # fc c # lc c
                                                                                                    # lwL 0.1 # fillRule EvenOdd

----------------------------------------------------------------------------------------------------------
-- detailed content, generated with the helper functions
----------------------------------------------------------------------------------------------------------

---------------
-- filesystem
---------------

unix_echo = commandWOptions (command "echo") (description ["display a line of text"])
                            [(c_option "-n" "do not output the trailing newline", []),
                             (c_option "-e" "enable interpreatation of backslash escapes", [])]

unix_ls = commandWOptions (command "ls") (description ["list content of directories"])
  [(c_option "-l" "detailed output",["drwxr-xr-x 11 rod rod 12288 Jun 23 13:33  Downloads",
                               --      "-rw-r--r-- 1  rod rod 179   Nov 9 2011    examples.desktop",
                                     "drwxrwxr-x 3  rod rod 4096  May 8  23:23  workspace",
                                     "-rw-rw-r-- 1  rod rod 481   Feb 18 12:12  X.ini"]),
   (c_option "-a or --a" "list all (also hidden files)",[
                                     "-rw-r--r-- 1  rod rod 179   Nov 9 2011    .config",
                                     "drwxr-xr-x 11 rod rod 12288 Jun 23 13:33  Downloads",
                                     "-rw-r--r-- 1  rod rod 179   Nov 9 2011    examples.desktop",
                                     "drwxrwxr-x 3  rod rod 4096  May 8  23:23  workspace",
                                     "-rw-rw-r-- 1  rod rod 481   Feb 18 12:12  X.ini"]),
   (c_option "-i" "prints inode number",["1594379 bin",
                                         "1594488 Deleted",
                                         "1594382 file2",
                                         "1594330 films"]),
   (c_option "-R" "list subdirectories' contents recursively",
          [".:",
           "bin deleted file2 films",
--           "./bin:aravind colour jace raman reader",
           "./deleted:dir1 dir2 dir3 file1 file2 file3 list.txt",
           "./deleted/dir1: ",
           "./deleted/dir2: dir",
--           "./deleted/dir2/dir: ",
--           "./deleted/dir3: ",
           "./films:action comedy horror",
           "./films/action:firstBlood",
--           "./films/comedy:supernaturalComedy",
--           "./films/comedy/supernaturalComedy:ghostbusters ghostbusters2",
--           "./films/horror:slasher zombie",
--           "./films/horror/slasher:halloween",
           "./films/horror/zombie:28DaysLater predator"])]

unix_cat = commandWOptions (command "cat") (description ["concatenate file contents and print them on the screen"])
  [(c_option "-E" "end each line with $",
  ["Remember, remember$",
   "The fifth of November!$",
   "Gunpowder, treason, and plot.$",
   "I see no reason why the gunpowder treason$",
   "should ever be forgot.$"]), (c_option "-s" "only the first one of repeated empty lines is printed",["#!/bin/bash",
   "",
   "clear",
   "while [ true ]; do",
   " sleep 1",
   " echo -e \"I am the shit!\"",
   "done",
   "",
   "echo -e \"I stopped being the shit.\""]),
  (c_option "-n" "numbers the lines printed",
  ["1 Remember, remember",
   "2 The fifth of November!",
   "3 Gunpowder, treason, and plot.",
   "4 I see no reason why the gunpowder treason",
   "5 should ever be forgot."])]

unix_touch = commandDescriptionExample (command "touch")
                                       (description ["creates new files"])
                                       ["touch file{1,2,3}","ls","file1 file2 file3"]

unix_mkdir = commandWOptions (command "mkdir") (description ["creates directories"])
                             [(c_option "-p" "creates directories with parent directories in one go", []),
                              (c_option "-v" "print a message for each creatad directory (to stderr)", [])]

unix_rmdir = commandWOptions (command "rmdir") (description ["deletes directories if they are empty"])
                             [(c_option "-p" "removes directories and their empty subdirectories in one go", []),
                              (c_option "-v" "output a diagnostic for every directory processed", [])]

unix_cd = commandDescriptionExample (command "cd") (description ["change to directory"]) ["cd ~  # change to the home directory"]

unix_rm = commandWOptions (command "rm") (description ["deletes files"])
                          [(c_option "-r" "deletes directories and their contents/subdirectories recursively", []),
                           (c_option "-i" "asks before every deletion", []),
                           (c_option "-v" "explain what is being done", [])]

unix_cp = commandWOptions (command "cp") (description ["copies files"])
                          [(c_option "-r" "copies directories and their contents/subdirectories recursively", []),
                           -- (c_option "-s" "results are not copies but symbolic links", []),
                           (c_option "-b" "makes backups of existing destination files first", [])]

unix_mv = commandWOptions (command "mv") (description ["moves or renames files"])
                          [(c_option "-i" "asks before an existing file is overwritten", [])]

unix_ln = commandWOptions (command "ln") (description ["creates file links"])
                          [(c_option " " "no option results in hard links", []),
                           (c_option "-l" "makes symbolic instead of hard links", []),
                           (c_option "-t" "puts the generated links into a specified directory", [])]

unix_stat = commandWOptions (command "stat") (description ["prints information on file status"])
                            [(c_option "" "",
  [" File: `wordList'", " Size: 6338 Blocks: 16 IO Block: 4096 regular file",
   "Device: fd01h/64769d Inode: 1594466 Links: 1",
   "Access: (0670/-rw-rwx---) Uid: ( 1964/auric.goldfinger) Gid: ( 100/users)",
   "Access: 2012-07-13 11:09:46.000000000 +0100",
   "Modify: 2012-06-22 13:18:01.000000000 +0100",
   "Change: 2012-07-09 15:00:10.000000000 +0100"]),

  (c_option "-t" "information is given in terse form",
   ["wordList 6338 16 81b8 2209 100 fd01 1594466",
    "1 0 0 1342174186 1340367481 1341842410 4096"]),
  (c_option "-f" "status of file system instead of file is reported",
   ["File: \"wordList\""," ID: adc9b05614460536 Namelen: 255 Type: ext2/ext3",
    "Block size: 4096 Fundamental block size: 4096",
    "Blocks: Total: 7240958 Free: 5622141 Available: 5254321",
    "Inodes: Total: 1839600 Free: 1702146"])]

unix_tree = commandWOptions (command "tree") (description ["displays directory contents as tree"])
 [(c_option "" "",
  ["films",
   "|-- action",
   "| `-- firstBlood",
   "|-- comedy",
   "| `-- supernaturalComedy",
   "| |-- ghostbusters",
   "| `-- ghostbusters2",
   "`-- horror",
   " |-- slasher",
--   " | `-- halloween",
--   " `-- zombie",
--   " |-- 28DaysLater",
--   " `-- predator",
   "",
   "6 directories, 6 files"]),
  (c_option "-d" "only directories", --  are considered",
  ["films",
   "|-- action",
   "|-- comedy",
   "| `-- supernaturalComedy",
   "`-- horror",
   " |-- slasher",
   " `-- zombie",
   "",
   "6 directories"]),

  (c_option "-a" "entries starting with '.'",
  [".",
--   "|-- .asd.swp",
   "|-- .bash_history",
   "|-- .bash_profile",
   "|-- .bashrc",
--   "|-- .qwetzi.swp",
--   "|-- .script1.swp",
--   "|-- .viminfo",
--   "|-- .vimrc",
--   "|-- Deleted",
--   "|-- bin",
--   "| |-- aravind",
--   "| |-- colour",
--   "| |-- jace",
--   "| |-- raman",
   "|-- deleted",
   "| |-- dir1",
   "| |-- file1",
--   "| |-- file2",
--   "| |-- file3",
--   "| `-- list.txt",
--   "|-- file2",
   "",
   "4 directories, 18 files"]),
  (c_option "-u" "prints username", -- of the files",
  ["films",
   "|-- [nerd.gee] action",
   "| `-- [nerd.gee] firstBlood",
   "|-- [nerd.gee] comedy",
   "| `-- [nerd.gee] supernaturalComedy",
--   "| |-- [nerd.gee] ghostbusters",
   "| `-- [nerd.gee] ghostbusters",
-----   "`-- [nerd.gee] horror",
   " |-- [nerd.gee] slasher",
--   " | `-- [nerd.gee] halloween",
--   " `-- [nerd.gee] zombie",
--   " |-- [nerd.gee] 28DaysLater",
--   " `-- [nerd.gee] predator",
   "",
   "6 directories, 6 files"]),
  (c_option "-l" "follows symbolic links", []),
  (c_option "--inodes" "prints inode number",
  ["films",
   "|-- [1594337] action",
   "| `-- [1594346] firstBlood",
   "|-- [1594336] comedy",
   "| `-- [1594349] supernaturalComedy",
   "| `-- [1594344] ghostbusters",
   " |-- [1594335] slasher",
   "",
   "6 directories, 6 files"])]

unix_pwd = commandDescriptionExample (command "pwd")
                                     (description ["prints name of current directory"])
                                     ["/home/auric.goldfinger/projects/secret/grandslam"]

unix_find = commandVerticalOptions (command "find") (description ["looks for files"])
 [ (c_option "find auxFile" "looks for files named \"auxFile\" in the current directory only",
     ["find: auxFile: No such file or directory"]),
   (c_option "find -name auxFile" "looks for files named \"auxFile\" in the current directory and its subdirectories",
     ["./subdir1/thatDir/auxFile", "./subdir1/auxFile"]),
   (c_option "find -maxdepth 2 -name auxFile"
             "looks for files named \"auxFile\" in the (sub)directories up to level 2 (current directory being level 1)",
     ["./subdir1/auxFile"]),
   (c_option "find /home -maxdepth 1 -name \"james*\""
             "looks for files/directories whose names start with \"james\" in the directory '/home' (first level only)",
     ["/home/james.bond",
      "/home/james.brown",
      "/home/james.cook",
      "/home/james.kirk"]),
   (c_option "find -empty" "returns only directories/regular files that are empty", ["./subdir1/dirWithoutContent"]),
   (c_option "-type l" "returns only symbolic links",["./subdir1/auxFile"]),
   (c_option "find -amin +25" "returns only files/directories accessed more than 25 minutes ago",
     [".", "./subdir1/auxFile"]),
   (c_option "find -atime -2" "returns only files/directories accessed less than 48 hours ago",
     [".", "./subdir1", "./subdir1/thatDir",
      "./subdir1/thatDir/auxFile",
      "./subdir1/auxFile"]),
   (c_option "find -mmin 100" "returns only files/directories with data modified exactly 100 minutes ago", ["./subdir1"]),
   (c_option "find -perm 700" "returns only files/directories where owner has all permissions, group and others have no permission",
     ["./subdir1/dirWithoutContent"]),
   (c_option " find -name *Content -delete" "finds all files/directories whose names end with 'Content' and deletes them",[]),
   (c_option "find /bin/ch* -exec wc '{}' \\;"
             "finds all files with names starting with 'ch' in directory '/bin' and executes command 'wc' on them",
     [" 134 1381 56984 /bin/chgrp",
      " 110 1183 54112 /bin/chmod",
      " 148 1489 59180 /bin/chown"]),
   (c_option "find -name auxFile -type f -exec chmod go+r '{}' \\;"
             "finds all regular files named 'auxFile' and adds read permission for group and others on those files",[]) ]


unix_df = commandWOptions (command "df") (description ["reports usage of disk space by file system"])
 [(c_option "" "",
  ["Filesystem 1K-blocks Used Available Use% Mounted on",
   "/dev/mapper/VolGroup00-LogVol00",
   " 10063176 7788044 1763952 82% /",
   "/dev/mapper/VolGroup00-LogVol02",
   " 28963832 6533316 20959236 24% /home",
   "/dev/sda1 194442 18711 165692 11% /boot",
   "tmpfs 517380 96 517284 1% /dev/shm"]),
  (c_option "-h" "sizes are given in easily understandable units",
  ["Filesystem Size Used Avail Use% Mounted on",
   "/dev/mapper/VolGroup00-LogVol00 9.6G 7.5G 1.7G 82% /",
   "/dev/mapper/VolGroup00-LogVol02 28G 6.0G 21G 23% /home",
   "/dev/sda1 190M 19M 162M 11% /boot",
   "tmpfs 506M 96K 506M 1% /dev/shm" ]),
  (c_option "-i" "information on inodes instead of sizes is given",
  [ "Filesystem Inodes IUsed IFree IUse% Mounted on",
    "/dev/mapper/VolGroup00-LogVol00 638976 136235 502741 22% /",
    "/dev/mapper/VolGroup00-LogVol02 1839600 135016 1704584 8% /home",
    "/dev/sda1 50200 40 50160 1% /boot",
    "tmpfs 129345 3 129342 1% /dev/shm" ]) ]

unix_du = commandWOptions (command "du") (description ["estimates space usage by directories"])
 [(c_option "" "",
  ["8 ./dir",
   "8 ./films/horror/zombie",
   "8 ./films/horror/slasher",
   "24 ./films/horror",
   "8 ./films/action",
   "168 ."]),
  (c_option "-s" "display total space usage only", ["168."]),
  (c_option "--time" "give also last modifaction time",
   ["8 2012-07-03 14:17 ./dir",
    "8 2012-06-20 12:38 ./films/horror/zombie",
    "8 2012-06-20 12:38 ./films/horror/slasher",
    "24 2012-06-20 12:38 ./films/horror",
    "168 2012-07-03 14:17 ."]) ]


unix_quota = commandDescriptionExample (command "quota")
                                       (description ["reports usage of disk space by and limits for the user"])
     ["Disk quotas for user hugo.drax (uid 1979):",
      " Filesystem blocks quota limit grace files quota limit grace",
      "/dev/mapper/VolGroup00-LogVol02",
      " 820 5000 5000 173 00"]

unix_chmod = commandVerticalOptions (command "chmod") (description ["sets/changes file permissions"])
 [(c_option "chmod u+rw myFile" "adds read and write permission for file \"myFile\" to the owners permissions", []),
  (c_option "chmod g-wx myFile" "removes write and execute permission for file \"myFile\" from the groups' permissions", []),
  (c_option "chmod o-wx myFile" "removes write and execute permission for file \"myFile\" from the others' permissions", []),
  (c_option "chmod a=r myFile" "sets the permissions for myFile to read for all owner, group, and others (and nothing else)", []),
--  (c_option "chmod 754 myFile" "sets the permissions for myFile to read, write, and execute for the owner, read and execute for the owner's group, and read for others", []),
--  (c_option "-v" "gives a short message after setting permissions", ["mode of `d1/d2/myFile' retained as 0754 (rwxr-xr--)"]),
--  (c_option "-c" "gives a short message only if changes are made", ["mode of `d1/d2/myFile' changed to 0777 (rwxrwxrwx)"]),
  (c_option "-R" "sets permissions on files and directories recursively", [])]

unix_permissions = textBox
  ["chmod UGO myfile (User Group Other)",
   "eg: chmod 754 myfile",
   "where each octal number stands for rwx:",
--   "000 = 0 none",
   "001 = 1 = --x = execute only",
   "010 = 2 = -w- = write only",
   "011 = 3 = -wx = write and execute",
   "100 = 4 = r-- = read only",
   "101 = 5 = r-x = read and execute",
   "110 = 6 = rw- = read and write",
   "111 = 7 = rwx = full"
   ] 1.3 white grey 0.3 # alignTL


-----------------------------
-- Processing of Lines
-----------------------------

unix_wc = commandWOptions (command "wc") (description ["counts lines, words, characters"])
  [(c_option "" "", ["80 171 935 hangman"]),
   (c_option "-l" "number of lines", ["80 hangman"]),
   (c_option "-w" "number of words", ["171 hangman"]),
   (c_option "-c" "number of characters",["935 hangman"])]

unix_cut = commandVerticalOptions (command "cut") (description ["selects and displays sections from lines in a file"])
   [(c_option "cut -c1-7 PrimeMinisters" "selects the 1st to 7th character of every line in file\"PrimeMinisters\"",
     [--"Maggie",
      --"John Ma",
      "Tony Bl",
      "Gordon",
      "David C"]),
    (c_option "cut -f1 -d\"-\" PrimeMinisters" "selects 1st field of every line in file \"PrimeMinisters\", interpreting '-' as field delimiter",
     [--"Maggie Thatcher|1979",
      --"John Major|1990",
      "Tony Blair|1997",
      "Gordon Brown|2007",
      "David Cameron|2010"]),
    (c_option "cut -f1,3 -d \"|\" --output-delimiter=\"; \" PrimeMinisters"
  "selects 1st and 3rd field of every line in file \"PrimeMinisters\", interpreting '|' as field delimiter and replacing it by '; ' for the output",   
     [--"Maggie Thatcher; Tories",
      --"John Major; Tories",
      "Tony Blair; Labour",
      "Gordon Brown; Labour",
      "David Cameron; Tories"]) ]


unix_grep = commandVerticalOptions (command "grep") (description ["searches in files for lines matching a specified pattern, and prints the results"])
  [ (c_option "grep Tories PrimeMinisters" "searches for lines containing 'Tories' in file \"PrimeMinisters\"",
     ["Maggie Thatcher|1979-1990|Tories|1979, 1983, 1987",
      "John Major|1990-1997|Tories|1992 elections",
      "David Cameron|2010-20??|Tories|2010 elections"]),
    (c_option "grep -c Labour PrimeMinisters" "searches for 'Labour' in file \"PrimeMinisters\", prints only a count of matching lines found",["2"]),
    (c_option "grep -f FourLetterNames PrimeMinisters" "searches in file \"PrimeMinisters\" for patterns read from file \"FourLetterNames\"",
     ["John Major|1990-1997|Tories|1992 elections",
      "Tony Blair|1997-2007|Labour|1997, 2001, 2005 elections"]),
    (c_option "grep -i G PrimeMinisters" "searches case-insensitive for 'G' in file \"PrimeMinisters\"",
     ["Maggie Thatcher|1979-1990|Tories|1979, 1983, 1987",
      "Gordon Brown|2007-2010|Labour|elections"]),
    (c_option "grep -n Tony PrimeMinisters" "searches for 'Tony' in file \"PrimeMinisters\", prints also the numbers of the lines found",
     ["3:Tony Blair|1997-2007|Labour|1997, 2001, 2005 elections"]),
    (c_option "grep -v Tories PrimeMinisters" "selects and prints only those lines of file \"PrimeMinisters\" not matching the pattern 'Tories'",
     ["Tony Bliar|1997-2007|Labour|1997, 2001, 2005 elections",
      "Gordon Brown|2007-2010|Labour|elections"]) ]


unix_tee = commandWOptions (command "tee") (description ["writes input to stdout and a file at the same time"])
  [ (c_option "tee tFile" "writes the input of the user into file \"tFile\" as well as to the terminal",
    ["Hello, this is me writing some text.",
     "Hello, this is me writing some text.",
     "Why did that repeat?",
     "Why did that repeat?",
--     "Ah, I see - it's because I use 'tee'!",
--     "Ah, I see - it's because I use 'tee'!",
     "^C"]),
    (c_option "tee -a tFile" "appends the input to file \"tFile\" instead of overwriting it",
    ["And more text. Will this be added to the content of \"tFile\" instead of replacing it?",
     "And more text. Will this be added to the content of \"tFile\" instead of replacing it?",
     "Yes it does!",
--     "Yes it does!",
     "^C"]) ]


unix_tail = commandWOptions (command "tail") (description ["prints the last 10 lines to stdout"])
  [ (c_option "" "", ["line 19", ":","line 29"]),
    (c_option "-n" "prints the last n lines", ["line 19",":","line 19+n"]) ]

unix_head = commandWOptions (command "head") (description ["prints the first 10 lines to stdout"])
  [ (c_option "" "", ["line 1", ":","line 10"]),
    (c_option "-n" "prints the first n lines", ["line 1", ":","line n"]) ]

unix_more = commandWOptions (command "more") (description ["loads a whole file and prints its content to the screen one page at a time"]) []

unix_less = commandWOptions (command "less") (description ["prints a file's content to the screen in a scrollable view, loading only as much of the file as needed"]) []

unix_sort = commandVerticalOptions (command "sort") (description ["prints sorted file contents to stdout"])

 [ (c_option "sort Beatles" "sorts lines in file \"Beatles\" and prints them out",
               ["Harrison, George, 1943, 2001",
                "Lennon, John, 1940, 1980",
                "McCartney, Paul, 1942, 20??",
                "Starr, Ringo, 1940, 20??"]),
   (c_option "sort -r Beatles" "sorts lines in file \"Beatles\" in reverse order and prints them out",
               ["Starr, Ringo, 1940, 20??",
                "McCartney, Paul, 1942, 20??",
                "Lennon, John, 1940, 1980",
                "Harrison, George, 1943, 2001"]),

   (c_option "sort -c Beatles" "does not sort but check if lines in file \"Beatles\" are sorted",
               ["sort: Beatles:3: disorder: Harrison, George, 1943, 2001"]),

--   (c_option "-b" "leading blanks are ignored", ["???"]),

   (c_option "sort -n Commodities" "sorts lines of file \"Commodities\" numerically",
               ["27.5$/oz.       Silver",
                "106.5$/bbl.     Oil",
                "1600$/oz.       Gold"]),

   (c_option "sort -k 2 Commodities" "sorts lines of file \"Commodities\", the sort key is understood to start 2nd field",
               ["1600$/oz.       Gold",
                "106.5$/bbl.     Oil",
                "27.5$/oz.       Silver"]) ]

unix_uniq = commandVerticalOptions (command "uniq") 
                                   (description ["prints lines to stdout omitting those who are copies of their predecessor"])
  [ (c_option "uniq USPresidentialElectionWinners" "prints lines of file \"USPresidentialElectionWinners\" to stdout omitting repetitions",
     ["George Bush",
      "Bill Clinton",
      "George W. Bush",
      "Barack Obama"]),
    (c_option "uniq -c USPresidentialElectionWinners"
              "prints non-double lines of file \"USPresidentialElectionWinners\" to stdout, together with the number of occurrences",
     ["1 George Bush",
      "2 Bill Clinton",
      "2 George W. Bush",
      "1 Barack Obama"]),
    (c_option "uniq -u USPresidentialElectionWinners" "prints only non-repeated lines of file \"USPresidentialElectionWinners\"",
     ["George Bush",
      "Barack Obama"]),
    (c_option "uniq -d USPresidentialElectionWinners" "prints only repeated lines of file \"USPresidentialElectionWinners\"",
     ["Ronald Reagan",
      "Bill Clinton",
      "George W. Bush"])
  ]


{-
unix_more = commandWOptions [command "more"] [description "prints a file's content to the screen one page at a time"] []
--  [ (c_option "-s" "prints only one of repeated blank lines", ["???"]),
--    (c_option "-num 11" "the size of a page is understood to be 11 lines", ["???"]) ]

unix_less = commandWOptions [command "less"] [description "prints a file's content to the screen in a scrollable view"] []
--  [ (c_option "-s" "prints only one of repeated blank lines", ["???"]),
--    (c_option "-S" "the end of lines too long for the screen is not printed", ["???"]),
--    (c_option "-w" "the first line not shown before is highlighted if a whole next page is shown", ["???"]),
--    (c_option "-E" "makes 'less' exit the first time that end-of-file is reached", ["???"]) ]


unix_sort = commandVerticalOptions [command "sort"] [description "prints sorted file contents to stdout"] 
 [ (c_option "-n" "sorts numerically",["???"]),
   (c_option "-r" "sorts in reverse order", ["???"]),
   (c_option "-b" "leading blanks are ignored", ["???"]),
   (c_option "-c" "does not sort but check if input is sorted", ["???"]),
   (c_option "-k 3.1,7,3" "the sort key is understood to start at 1st character of 3rd field and end at 3rd character of 7th field", ["???"]) ]

unix_uniq = commandWOptions [command "uniq"] [description "prints lines to stdout omitting repetitions"]
  [ (c_option "-c" "prints also number of occurrences", ["???"]),
    (c_option "-u" "prints only non-repeated lines", ["???"]),
    (c_option "-d" "prints only repeated lines", ["???"]) ]
-}


-- unix_tr = commandWOptions (command "tr") (description ["replaces specified characters by given substitutes"])
--  [ (c_option "-d" "specified characters are not replaced but deleted", ["???"]),
--    (c_option "-s" "any repetition of a specified character is replaced by only one substitute", ["???"]) ]

unix_tr = commandVerticalOptions (command "tr") (description ["replaces specified characters by given substitutes"])
  [ (c_option "echo \"Mind the gap.\" | tr Mg Fm?"
              "replaces 'M' by 'F' and 'g' by 'm' in \"Mind the gap.\"", ["Find the map."]),
    (c_option "echo \"West End\" | tr -d sn"
              "specified characters 's', 'n' are not replaced but deleted in \"West End\"", ["Wet Ed"]),
    (c_option "echo \"Summer 1999\" | tr -s m9 p%"
              "any sequence of 'm's, '9's in \"Summer 1999\" is replaced by one 'p' or '%', respectively",
--     "any repetition of a specified character is replaced by only one substitute",
     ["Super 1%"]) ]

unix_cmp = commandVerticalOptions (command "cmp") (description ["compares two files"])
  [ (c_option "" "", ["file1 file2 differ: byte 94, line 7"]),
    (c_option "-l" "prints byte numbers and divergent byte values for every difference to stdout",
   ["94 122 124",
    " 95 157 151",
    " 96 147 155",
    " 97 145 157",
--    " 99 40 150",
--    "101 157 40",
--    "102 157 104",
    "cmp: EOF on file2"]),
    (c_option "-s" "only returns exit status and prints nothing", []) ]

unix_diff = commandWOptions (command "diff") (description ["reports differences between two files"])
  [ (c_option "" "",
   ["1c1",
    "< AbcdEfghIjklmnOpqrstUvwxyz",
    "---",
    "> abcdefghijklmnopqrstuvwxyz"]),
    (c_option "-i" "differences in case are ignored", []),
    -- (c_option "-q" "reports only if files differ but not the differences", ["Files alphabet and capitalVowelAlphabet differ"]),
    (c_option "-s" "reports if files do not differ", ["Files alphabet and letters are identical"]) ]

unix_rev = commandVerticalOptions (command "rev") (description ["displays characters in reverse order line-by-line"])
  [ (c_option "rev " "EinsteinQuote", ["hguone llew ti dnatsrednu t'nod uoy ylpmis ti nialpxe t'nac uoy fI"] ) ]

unix_finger = commandWOptions (command "finger") (description ["displays information about users currently logged in"])
  [ (c_option "" "",
     ["Login Name Tty Idle Login Time Office Office Phone", "theboy.wholived Harry Potter pts/10 Jul 18 11:17 (abc0112358.fdmgroup.local) ",
      "youknow.who Tom Riddle pts/2 Jul 18 11:03 (xyz235711.fdmgroup.local) ",
      "albus.dumbledore Albus Dumbledore pts/0 8 Jul 18 09:56 (headmaster.hogwarts.wiz)"]),
    (c_option "-l" "gives more detailed information about users",
     ["Login: ron.wea Name: Ronald+ Weasley",
      "Directory: /home/ron.wea Shell: /bin/bash",
      "On since Wed Jul 18 09:56 (BST) on pts/0 from 11.5.9.139",
      " 14 minutes 21 seconds idle",
      "No mail.",
      "No Plan."
     ]) ]

unix_who = commandWOptions (command "who") (description ["shows users logged on"])
  [ (c_option "" "",
    ["michael.jackson pts/0 2009-06-25 09:56 (11.2.7.151)",
     "james.roberts pts/4 2012-07-18 10:54 (abc0815.fdmgroup.local)",
     "jack.rackham pts/10 2012-07-18 11:17 (lapfrk2012.fdmgroup.local)"]),
    (c_option "-q" "displays usernames and a count",
    ["michael.jackson", "james.roberts",
     "jack.rackham  # users=3"]),
    (c_option "-b" "displays time of last system boot",
    ["system boot 2012-05-27 11:47"]) ]

unix_sudo = commandDescriptionExample (command "sudo") (description ["execute the following command as a superuser"])
   ["sudo rm -r *    [sudo] password for till:"]

unix_su = commandDescriptionExample (command "su") (description ["become the superuser for a session"])
   ["su              password:",
    "rm -r *"]

unix_which = commandWOptions (command "which") (description ["shows the path of executable files associated to commands"])
  [ (c_option "" "", ["/usr/bin/finger"]),
    (c_option "-a" "shows all paths, not only the first one", ["/bin/cut", "/usr/bin/cut"]) ]

unix_env = commandDescriptionExample (command "env")
                                     (description ["print the environment variables to the screen"])
      ["HOSTNAME=unix",
--       "TERM=xterm",
       "SHELL=/bin/bash",
       "USER=you.there"
--       ".",
--       "."
      ]

----------------------------
-- Processes
----------------------------

unix_ps = commandWOptions (command "ps") (description ["displays a table of current processes"])
 [(c_option "" "",
  [" PID TTY TIME CMD",
   " 7319 pts/22 00:00:00 bash",
   "23321 pts/22 00:00:00 ps"]),
  (c_option "-e" "list all processes",
  ["PID TTY TIME CMD",
   " 1 ? 00:00:07 init",
   " 2 ? 00:00:00 kthreadd",
--   " 3 ? 00:00:00 migration/0",
--   " 4 ? 00:00:22 ksoftirqd/0",
--   " 5 ? 00:00:00 watchdog/0",
--   " 6 ? 00:00:11 events/0",
--   " 7 ? 00:00:00 khelper",
--   " 64 ? 00:00:01 kblockd/0",
--   " 66 ? 00:00:00 kacpid",
--   " 67 ? 00:00:00 kacpi_notify",
--   " 123 ? 00:00:00 cqueue",
--   " 125 ? 00:00:00 ksuspend_usbd",
--   ".",
   ".",
   ".",
   "25139 ? 00:00:00 script",
   "25140 pts/61 00:00:00 bash"]),
  (c_option "-f" "gives a full format listing",
  ["UID PID PPID C STIME TTY TIME CMD",
   "1234 7319 7318 0 09:40 pts/22 00:00:00 -bash",
   "1234 25852 7319 1 11:35 pts/22 00:00:00 ps -f"]) ]

unix_top = commandWOptions (command "top") (description ["displays dynamic real-time view of current Linux tasks"])
 [ -- (c_option "" "", [])
--  [" PID USER PR NIVIRT RES SHR S %CPU %MEM TIME+ COMMAND",
--   "25135 bart.sim 20 0 5660 1020 888 S 1.0 0.1 3:54.50 sh",
--   " 584 root 15 -5 0 0 0 S 0.5 0.0 14:31.59 ata/0",
--   " 2453 root 20 0 18812 4432 1764 S 0.5 0.4 13:29.12 Xorg",
--   "30257 homer.si 20 0 2496 1096 808 R 0.5 0.1 0:00.30 top",
--   "30281 ned.flan 20 0 12092 3188 2256 S 0.5 0.3 0:00.13 vim",
--   "30316 edna.kra 20 0 12064 3176 2236 S 0.5 0.3 0:01.23 vim",
--   " 1 root 20 0 1948 476 424 S 0.0 0.0 0:05.74 init",
--   " 2 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 kthreadd",
--   " 3 root RT -5 0 0 0 S 0.0 0.0 0:00.00 migration/0",
--   " 4 root 15 -5 0 0 0 S 0.0 0.0 0:15.64 ksoftirqd/0",
--   " 5 root RT -5 0 0 0 S 0.0 0.0 0:00.20 watchdog/0",
--   " 6 root 15 -5 0 0 0 S 0.0 0.0 0:08.57 events/0",
--   " 7 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 khelper",
--   " 64 root 15 -5 0 0 0 S 0.0 0.0 0:01.60 kblockd/0",
--   " 66 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 kacpid",
--   " 67 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 kacpi_notify",
--   " 123 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 cqueue",
--   " 125 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 ksuspend_usbd",
--   " 130 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 khubd",
--   " 133 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 kseriod",
--   " 173 root 20 0 0 0 0 S 0.0 0.0 0:00.00 pdflush",
--   " 174 root 20 0 0 0 0 S 0.0 0.0 0:14.72 pdflush"]),
  (c_option "-d 1.5" "sets the delay between screen updates to 1.5 seconds", [] )]
--  [" PID USER PR NI VIRT RES SHR S %CPU %MEM TIME+ COMMAND",
--   "25135 bart.sim 20 0 5660 1020 888 S 1.0 0.1 3:54.50 sh",
--   " 584 root 15 -5 0 0 0 S 0.5 0.0 14:31.59 ata/0",
--   " 2453 root 20 0 18812 4432 1764 S 0.5 0.4 13:29.12 Xorg",
--   "30257 homer.si 20 0 2496 1096 808 R 0.5 0.1 0:00.30 top",
--   "30281 ned.flan 20 0 12092 3188 2256 S 0.5 0.3 0:00.13 vim",
--   "30316 edna.kra 20 0 12064 3176 2236 S 0.5 0.3 0:01.23 vim",
--   " 1 root 20 0 1948 476 424 S 0.0 0.0 0:05.74 init",
--   " 2 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 kthreadd",
--   " 3 root RT -5 0 0 0 S 0.0 0.0 0:00.00 migration/0",
--   " 4 root 15 -5 0 0 0 S 0.0 0.0 0:15.64 ksoftirqd/0",
--   " 5 root RT -5 0 0 0 S 0.0 0.0 0:00.20 watchdog/0",
--   " 6 root 15 -5 0 0 0 S 0.0 0.0 0:08.57 events/0",
--   " 7 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 khelper",
--   " 64 root 15 -5 0 0 0 S 0.0 0.0 0:01.60 kblockd/0",
--   " 66 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 kacpid",
--   " 67 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 kacpi_notify",
--   " 123 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 cqueue",
--   " 125 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 ksuspend_usbd",
--   " 130 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 khubd",
--   " 133 root 15 -5 0 0 0 S 0.0 0.0 0:00.00 kseriod",
--   " 173 root 20 0 0 0 0 S 0.0 0.0 0:00.00 pdflush",
--   " 174 root 20 0 0 0 0 S 0.0 0.0 0:14.72 pdflush"])]

unix_jobs = commandWOptions (command "jobs") (description ["lists active jobs"])
  [(c_option "-l" "gives also process IDs",  ["[1]+  8020 runs     find / -name \"abc\" &"]),
   (c_option "-r" "lists only running jobs", ["[1]+  runs     find / -name \"abc\" &"]),
   (c_option "-s" "lists only stopped jobs", [])
  ]

unix_fork = commandVerticalOptions (command "fork")
          (description ["SYSTEM CALL: generates a child process as 'copy' of the current one, its parent"])
       [ (c_option "" "1st system call in the course of creating a (child) process", []) ]

unix_wait = commandVerticalOptions (command "wait")
          (description ["SYSTEM CALL: the calling process 'goes to sleep' in order to allow its child(ren) to do their work"])
       [ (c_option "" "2nd system call in the course of creating a (child) process", []) ]

unix_exec = commandVerticalOptions (command "exec")
          (description ["SYSTEM CALL: a process starts its work, the execution of its associated command"])
       [ (c_option "" "3rd system call in the course of creating a (child) process", []) ]

unix_exit = commandVerticalOptions (command "exit")
  (description
    ["SYSTEM CALL: a process terminates after doing its work, causing its parent to 'wake up', and returning an integer as exit status"])
  [ (c_option "" "4th and last system call in the course of creating a (child) process", []) ]

unix_fg = commandDescriptionExample (command "fg")
                                    (description ["resumes execution of a suspended process by bringing it to the foreground and thus",
                                                   "redirecting its standard input and output streams to the user's terminal"])
                                    ["fg %1  # sends job #1 to the foreground"]

unix_bg = commandWOptions (command "bg")
        (description ["After suspending a job with [CTRL-Z], resumes execution",
                      "of the suspended process, leaving it in the background"]) []

unix_and = commandDescriptionExample (command "&") (description ["puts the command before in the background"]) ["find / -name \"abc\" &"]

unix_kill = commandWOptions (command "kill")  (description ["terminates processes specified by process Ids"])
  [ (c_option "-l" "lists all signals that 'kill' can send",
    [" 1) SIGHUP 2) SIGINT 3) SIGQUIT 4) SIGILL",
     " 5) SIGTRAP 6) SIGABRT 7) SIGBUS 8) SIGFPE",
     " 9) SIGKILL 10) SIGUSR1 11) SIGSEGV 12) SIGUSR2",
     "13) SIGPIPE 14) SIGALRM 15) SIGTERM 16) SIGSTKFLT",
     "17) SIGCHLD 18) SIGCONT 19) SIGSTOP 20) SIGTSTP",
     "21) SIGTTIN 22) SIGTTOU 23) SIGURG 24) SIGXCPU",
     "25) SIGXFSZ 26) SIGVTALRM 27) SIGPROF 28) SIGWINCH",
     "29) SIGIO 30) SIGPWR 31) SIGSYS 34) SIGRTMIN"]),
--     "35) SIGRTMIN+1 36) SIGRTMIN+2 37) SIGRTMIN+3 38) SIGRTMIN+4",
--     "39) SIGRTMIN+5 40) SIGRTMIN+6 41) SIGRTMIN+7 42) SIGRTMIN+8",
--     "43) SIGRTMIN+9 44) SIGRTMIN+10 45) SIGRTMIN+11 46) SIGRTMIN+12",
--     "47) SIGRTMIN+13 48) SIGRTMIN+14 49) SIGRTMIN+15 50) SIGRTMAX-14",
--     "51) SIGRTMAX-13 52) SIGRTMAX-12 53) SIGRTMAX-11 54) SIGRTMAX-10",
--     "55) SIGRTMAX-9 56) SIGRTMAX-8 57) SIGRTMAX-7 58) SIGRTMAX-6",
--     "59) SIGRTMAX-5 60) SIGRTMAX-4 61) SIGRTMAX-3 62) SIGRTMAX-2",
--     "63) SIGRTMAX-1 64) SIGRTMAX"]),
    (c_option "-p" "simply prints the process IDs to stdout", []) ]

unix_killall = commandVerticalOptions (command "killall") (description ["terminates all processes with matching names"])
  [(c_option "-u" "terminates all processes of the specified user", []),
   (c_option "-w" "waits until all relevant processes are dead", []),
   (c_option "-l" "lists all signals that can be sent",
   ["HUP INT QUIT ILL TRAP ABRT IOT BUS FPE KILL USR1 SEGV USR2 PIPE ALRM TERM",
    "STKFLT CHLD CONT STOP TSTP TTIN TTOU URG XCPU XFSZ VTALRM PROF WINCH IO PWR SYS",
    "UNUSED"]) ]

unix_date  = commandWOptions (command "date") (description ["print date"]) []
unix_cal   = commandWOptions (command "cal") (description ["print calendar"]) []
unix_free  = commandWOptions (command "free") (description ["print free memory"]) []
unix_uptime = commandWOptions (command "uptime") (description ["print uptime"]) []
unix_clear = commandWOptions (command "clear") (description ["clears the screen"]) []

-------------------------------------
-- Scripting
-------------------------------------

scripting0 = highlightedText
 [" #!/bin/bash                              # this is called she-bang",
  " for x in 0 1 2 3 4 5 6 7 8 9; do #for every item x in the list do the following:",
  "    if [ $x -eq 0 ]; then                 #  if x = 0 ...",
  "                    echo -e \"$x reads zero \"",
  "    elif [[ $x -lt 3 || $x -eq 6 ]]; then #  ... else, if x < 3 or x = 6 ...",
  "                    echo -ne \"$x = three \"",
  "                    sleep 2",
  "                    echo -ne \"letters in one word\\n\"",
  "    elif [[ $x -ne 3 && $x -ne 8 && $x -ne 7 ]]; then # ... else, if x is not 3, 8 or 7 ...",
  "                    echo -e \"$x is a four-letter word\"",
  "    else       # ... else (any case left)",
  "                    echo -e \"$x is five characters long\"",
  "    fi                                    #  end of if branches",
  " done                                     #  done! loop is finished."
 ] 1.7 black "bash"


scripting1 = highlightedText
 ["while read line #  as long as ‘read line’ works (i. e., returns 0),",
  "do                                     #  do the following:",
  "  first=$(echo -n $line) | cut -c1",
  "  case $first in                       #  in the case that first is ...",
  "    \"[A-Z]\") echo -ne \"$line\\t starts with a capital letter\\n\";;     #  ... a capital letter",
  "    \"[a-z]\") echo -ne \"$line\\t starts with a lower case letter\\n\";;  #  ... a lower-case letter",
  "    \"[0-9].*\") echo -ne \"$line\\t starts with a number\\n\";;           #  ... a numerical",
  "    *) echo -ne \"$line\\t starts with a non-alphanumeric character\\n\";; #  ... anything else",
  "  esac                                 #  end of case branches",
  "done < fileWithLines # input for ‘read line’ (cf. above) is taken from file ‘fileWithLines’"
 ] 1.7 black "bash"


scripting2 = highlightedText
 ["x=1",
  "until [ $x -ge 999999 ]; do                 #  as long as NOT x >= 999999, do the following:",
  "                echo -e \"$x is not enough, bid more!\"",
  "                x=$[$x*10]",
  "done                                        #  done! loop is finished."
 ] 1.7 black "bash"


scripting3 = highlightedText
 ["Numerical comparison",
  "   if [ $x -eq 1 ]; then  // if x = 1     if [ $x -ne 2 ]; then # if not x = 2",
  "   if [ $x -ge 3 ]; then  // if x >= 3    if [ $x -gt 4 ]; then # if x > 4",
  "   if [ $x -le 5 ]; then  // if x <= 5    if [ $x -lt 6 ]; then # if x < 6",
  "String Comparison",
  "   if [ $w = \"London\"    ]; then # if w is exactly the string \"London\"",
  "   if [ $w != \"Brighton\" ]; then # if w is not exactly the string \"Brighton\""
 ] 1.7 black "bash"


dollarOps = header === strutY 2 === textBox
 [
  "$var returns the value of variable",
  "     var (but assignment is var=value)",
  "",
  "$?  exit status of the last command/process",
  "    (comparable to a return value), integer",
  "    in { 0, 1, ..., 255 } (?); \"true\" is 0,",
  "    \"false\" is 1 (in general, not 0);",
  "    an exit status of 0 means ‘success’, anything",
  "    different from 0 means ... well, something",
  "    else; display exit status with echo $?",
  "",
  "$#  number of parameters (strings separated by",
  "    blanks) given to the command/script/method",
  "",
  "$1, $2, ..., $9",
  "    value of 1st, 2nd, ..., 9th parameter given",
  "    to the command/script/method; works only for",
  "    1, ..., 9, not for numbers with two or more",
  "    digits (use shift operator in that case)",
  "    CAUTION: for a script abc.sh containing a",
  "    function xyz(), $1 can have different meanings",
  "    inside abc.sh - outside of xyz() (and any",
  "    other function), it means the 1st command",
  "    line parameter given to abc.sh, e. g., \"hello\"",
  "    in case of ‘sh abc.sh hello world’;",
  "    inside of xyz(), it means the 1st parameter",
  "    handed over to xyz(), e. g., \"world\" in case",
  "    of ‘xyz $2’. Analogously for $2, ..., $#, etc.",
  "",
  "$$  process ID of the current shell",
  "    (as displayed by ‘ps’)",
  "",
  "$@  a list of all parameters given to the command/",
  "    script/method, each one in its own \" \"",
  "",
  "$*  a list of all parameters given to the command/",
  "    script/method, all together in one pair of \" \"",
  "",
  "$!  process ID of the last",
  "    executed background command"
 ] 1.9 black white 0.1 # centerXY
 where  header = textLin "$?, $#, ..." 5 black # centerXY


unix__exit = commandWOptions (command "exit") (description ["ends the current shell"])
  [ (c_option "exit 66" "ends the current shell with an exit status of 66 (can be displayed by exceuting 'echo $?' afterwards)", []) ]

unix_sleep = commandWOptions (command "sleep") (description ["delay for a specified amount of time"]) []
unix_man = commandWOptions (command "man") (description ["manual for command"]) []
unix_info = commandWOptions (command "info") (description ["info about command"]) []
unix_history = commandWOptions (command "history") (description ["all the commands entered so far"]) []

---------------------------------------
-- file system tree
---------------------------------------

fileSystemTree :: Tree (String,String)
fileSystemTree = Node ("/","")
                   [Node ("home","home directory") [Node ("james","Frankfurt trainer") [],
                                                    Node ("test","") []
                                                   ],
                    Node ("etc","")  [Node ("conf","") []],
                    Node ("usr","")  [Node ("sbin","") []]
                   ]

-- fileSystemTree2 :: Tree [(String,Colour a)]
fileSystemTree2 c =
  Node [("/",black), ("root directory",c)]
  [ Node [("bin",black), ("binary files for commands like 'cat', 'ps', ...)",c)] [],
    Node [("boot",black), ("files needed for starting the system",c)] [],
    Node [("dev",black), ("device drivers, also contains the 'black hole' /dev/null",c)] [],
    Node [("etc",black), ("system-wide configuration files",c)] [Node [("profile",black)] []],
    Node [("home",black),("home directories",c)] [Node [("user",black),("access your own directory with ~",c)] [],
                                                  Node [(".bash_profile",black),("startup file",c)] [],
                                                  Node [(".bash_login",black),("startup file",c)] [],
                                                  Node [(".profile",black),("startup file",c)] [],
                                                  Node [(".bashrc",black),("startup file",c)] []
                                                 ],
    Node [("lib",black), ("shared library files, possibly kernel-related files",c)] [],
    Node [("mnt",black), ("for temporarily mounted file systems like cdrom",c)] [],
    Node [("proc",black), ("contains process files",c)] [],
    Node [("tmp",black), ("holds temporary files",c)] [],
    Node [("usr",black),("",c)]
      [Node [("sbin",black),("",c)] [],
       Node [("var",black), ("files with variable data",c)] [],
       Node [("sbin",black), ("...",c)] []
      ]
  ]

-- | Neat 2-dimensional drawing of a tree (from Data.Tree)
drawTreeDiagram tree = vcat' (with & sep .~ 0) $
        map (hcat' (with & catMethod .~ Distrib & sep .~ 0.5)) $
        map (map (\(str,c) -> (textBit str 1.5 c # alignTL))) $ drawDiagram tree

-- drawDiagram :: Tree [(String,a)] -> [[(String,a)]]
drawDiagram (Node x ts0) = x : drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
      [("|",black)] : shift [("`- ",black)] [("   ",black)] (drawDiagram t)
    drawSubTrees (t:ts) =
      [("|",black)] : shift [("+- ",black)] [("|  ",black)] (drawDiagram t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

