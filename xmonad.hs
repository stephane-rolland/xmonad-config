import XMonad
import XMonad.Config.Azerty

import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.EZConfig (additionalKeysP)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.SpawnOnce as SOnce
--import qualified XMonad.Actions.SpawnOn as SOn
import qualified Data.List as DL

strTerminalCommandLine = "urxvt -tr +sb -fg white -bg black -tint darkgrey -sh 75 -fade 140 -fadecolor darkgrey -pr black -pr2 white -fn \"xft:Ubuntu Mono:pixelsize=12,style=regular\""
strTerminalCommandLineBig = "urxvt -tr +sb -fg white -bg black -tint darkgrey -sh 75 -fade 140 -fadecolor darkgrey -pr black -pr2 white -fn \"xft:Ubuntu Mono:pixelsize=18,style=regular\""
myTerminal = strTerminalCommandLine -- "urxvt"

strTerminalInDirectory = \strDirectory -> strTerminalCommandLine ++ " -cd " ++ strDirectory 
strTerminalInDirectoryCloud = \strDirectory -> strTerminalCommandLine ++ " -cd ~/cloud"

myModKey = mod4Mask
altKey = mod1Mask
shiftKey = shiftMask
ctrlKey = controlMask

strWallpaperFile = "~/nixos-config/dotfiles/wallpaper/horus_bw.jpg"
strSetWallPaper = "feh --bg-scale " ++ strWallpaperFile

startupOperations :: X ()
startupOperations = do
	spawn "~/nixos-config/dotfiles/scripts/screenlayout/___-horus.sh"
	spawn strSetWallPaper
	spawnWorkspaces

myBorderWidth = 3

myFocusedBorderColor = "turquoise"

myWorkspaces = ["1","2","3","4","5","6","7","8","9"] ++ (map label myExtraWorkspaces)

data Wk = Wk {label :: String, abbreviation :: String, cdPath :: String} deriving Show

mkCustomWorkspace base name nb isBase ""   = Wk name (DL.intersperse ' ' $ take nb name) (if isBase then "~/" ++ base ++ "/" ++ name else "/" ++ name)
mkCustomWorkspace base name nb isBase path = Wk name (DL.intersperse ' ' $ take nb name) (if isBase then "~/" ++ base ++ "/" ++ realPath  else "/" ++ realPath)
	where realPath = if null path then name else path

isRiskDupplicate ('t':'r':'a':_) = True
isRiskDupplicate ('b':'o':'o':_) = True
isRiskDupplicate ('w':'e':'b':_) = True
isRiskDupplicate _ = False

mkwsArt name alt_path  = if isRiskDupplicate name then mkCustomWorkspace "art" name 5 True alt_path else mkCustomWorkspace "art" name 3 True alt_path
mkwsCloud name alt_path  = if isRiskDupplicate name then mkCustomWorkspace "cloud" name 5 True alt_path else mkCustomWorkspace "cloud" name 3 True alt_path
mkwsSystem name alt_path = if isRiskDupplicate name then mkCustomWorkspace "" name 5 False alt_path else mkCustomWorkspace "" name 3 True alt_path

myExtraWorkspaces = [

	  mkwsSystem 	"mnt" 		""
	, mkwsSystem 	"tmp" 		""
	, mkwsSystem 	"trash" 	""
  
	, mkwsCloud 	"cloud" 	"/"
	  
	, mkwsCloud 	"system" 	"" 
	, mkwsCloud 	"scripts" 	"nixos-config/dotfiles/scripts" 
	, mkwsCloud 	"monitoring" 	"" 
	, mkwsCloud 	"servers" 	""
	, mkwsCloud 	"suspend" 	""
	, mkwsCloud 	"nixos" 	"nixos-config"
	, mkwsCloud 	"dotfiles" 	"nixos-config/dotfiles"
	, mkwsCloud 	"xmonad" 	"nixos-config/dotfiles/xmonad"

	, mkwsCloud 	"docs" 		""
	, mkwsCloud 	"backups"	""
	, mkwsCloud 	"scanner"	""

	, mkwsCloud 	"notes" 	""
	, mkwsCloud 	"src" 		""
	, mkwsCloud 	"machinelearning" "" 
	, mkwsCloud 	"talks"         ""
	, mkwsCloud 	"training"      ""
	, mkwsCloud 	"books"         ""
	, mkwsCloud 	"booknotes"     "book-notes"
	, mkwsCloud    	"journal"       ""

	, mkwsCloud 	"organz"        "organ-z"
	, mkwsCloud 	"ideas"         ""
	, mkwsCloud 	"todo"          ""
	, mkwsCloud 	"routine"       ""
	, mkwsCloud  	"review"        ""
	, mkwsCloud 	"tracking"      ""
	, mkwsCloud 	"headspace"     ""

	, mkwsCloud 	"life"          ""
	, mkwsCloud 	"languages"     ""
	, mkwsCloud 	"zerowaste"     "zero-waste-vegan"
	, mkwsCloud 	"shopping"      ""

	, mkwsArt 	"art"           ""
	, mkwsArt 	"kov"           "music/composer-projects/kov"
	, mkwsArt 	"music"         ""
	, mkwsArt  	"images"        ""
	, mkwsArt 	"calligraphy"   ""
	]
			

prettyShowWk wk = label wk ++ ":\t\t" ++ abbreviation wk ++ "\t\t in: \t\t" ++ cdPath wk
showWorkspaces = "\n" ++ DL.concat (map (\wk -> "- " ++ prettyShowWk wk  ++ "\n") myExtraWorkspaces) ++ "\n"


mOperationWorkspace = \wk -> do
	let terminalInDirectory = strTerminalInDirectory (cdPath wk) 
	spawn $  terminalInDirectory
	windows $ W.greedyView (label wk)

spawnWorkspaces = do
	--mapM mOperationWorkspace (snd <$> myExtraWorkspaces)
	return ()

opDefault = kill  -- this is the type of X computations
opWk = mOperationWorkspace -- \ws -> (windows $ W.greedyView ws)  >>= (SOnce.spawnOnce $ strTerminalInDirectory ws)  
opWkShift = \wk -> (windows $ W.shift (label wk))

--todo:  check list of all keys for interesting stuffs here: http://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Util-EZConfig.html#g:3
addMyKeyMappings = \config -> additionalKeysP config  $ [ 
      ("M-c", spawn $ strTerminalCommandLineBig)
    , ("M-t", spawn "xmessage 'test combinations ok: t hasbeen recognized'") 
    , ("M-S-k", kill) 
    , ("M-v", spawn "pavucontrol") 
    , ("M-b", spawn "firefox" )
    , ("M-S-b", spawn "vivaldi" )
    , ("M-e", spawn "gedit")  
    , ("M-w", spawn $ "xmessage '" ++ showWorkspaces ++ "'")] ++
    [ ("M-g " ++ abbreviation wk, opWk wk) | wk <- myExtraWorkspaces ] ++ 
    [ ("M-S-g " ++ abbreviation wk, opWkShift wk) | wk <- myExtraWorkspaces ]
          
myConfig = addMyKeyMappings configBase
	where configBase = azertyConfig {
                               borderWidth = myBorderWidth
	                     , terminal = myTerminal	
                             , modMask = myModKey
                             , startupHook = startupOperations
                             , focusedBorderColor = myFocusedBorderColor
                             , workspaces = myWorkspaces
                             }


main = do
    xmonad $ myConfig 

