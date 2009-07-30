{- |

Simple Dialogs
==============

Exactly what it says on the box. This library contains a collection
of simple dialogs which can be thrown into a program with minimal
effort. All functions take a title and a message. The option function
also takes a list of options, given as strings.

-}
module Graphics.UI.Dialog.Simple
  ( showInfoDialog
  , showErrorDialog
  , showYesNoDialog
  , showOkCancelDialog
  , showTextDialog
  , showOptionDialog
  , showPasswordDialog
  ) where

import Graphics.UI.Gtk

-- | Show an informational message. Provides no options to the user other
--   than to dismiss it.
showInfoDialog :: String -> String -> IO ()
showInfoDialog t m = dialogOfType MessageInfo ButtonsOk t m >> return ()

-- | Like an information dialog, but with a more alarming icon.
showErrorDialog :: String -> String -> IO ()
showErrorDialog t m = dialogOfType MessageError ButtonsOk t m >> return ()

-- | Offers the user a choice of either yes or no.
showYesNoDialog :: String -> String -> IO Bool
showYesNoDialog = dialogOfType MessageQuestion ButtonsYesNo

-- | Offers the user the choice to cancel an action.
showOkCancelDialog :: String -> String -> IO Bool
showOkCancelDialog = dialogOfType MessageWarning ButtonsOkCancel

-- | Offers a single-line text input box. The user can cancel, which will
--   result in a return value of Nothing.
showTextDialog :: String -> String -> IO (Maybe String)
showTextDialog = do
    inputDialog (\dialog -> do
                    input <- entryNew
                    onEntryActivate input $ dialogResponse dialog ResponseOk
                    return input)
                (\input -> entryGetText input)

-- | Give the user a choice between multiple options.
showOptionDialog :: String -> String -> [String] -> IO (Maybe String)
showOptionDialog title prompt options = do
    inputDialog (\dialog -> do
                    input <- comboBoxNewText
                    mapM (comboBoxAppendText input) options
                    return input)
                (\input -> do
                    idx <- comboBoxGetActive input
                    let index = if idx < 0 then 0 else idx
                    return $ options !! index)
                title prompt

-- | Ask the user to enter a password. Returns Nothing if cancelled.
showPasswordDialog :: String -> String -> IO (Maybe String)
showPasswordDialog = do
    inputDialog (\dialog -> do
                    input <- entryNew
                    set input [entryVisibility := False]
                    onEntryActivate input $ dialogResponse dialog ResponseOk
                    return input)
                (\input -> entryGetText input)

dialogOfType :: MessageType -> ButtonsType -> String -> String -> IO Bool
dialogOfType msgType btnType title msg = do
    initGUI
    dialog <- messageDialogNew Nothing [] msgType btnType msg
    set dialog [windowTitle := title]
    response <- dialogRun dialog
    result <- case response of
                   ResponseOk  -> return True
                   ResponseYes -> return True
                   _           -> return False
    widgetDestroy dialog
    return result

inputDialog getInput getValue title prompt = do
    initGUI
    dialog <- dialogNew
    set dialog [ windowTitle := title ]

    dialogAddButton dialog stockOk ResponseOk
    dialogAddButton dialog stockCancel ResponseCancel

    windowSetTypeHint dialog WindowTypeHintDialog
    windowSetKeepAbove dialog True
    windowSetSkipTaskbarHint dialog True
    windowSetSkipPagerHint dialog True
    dialogSetDefaultResponse dialog ResponseOk

    label <- labelNew $ Just prompt
    input <- getInput dialog

    upper <- dialogGetUpper dialog
    boxPackStart upper label PackGrow 10
    boxPackStart upper input PackGrow 10
    widgetShowAll upper

    response <- dialogRun dialog
    result <- case response of
                   ResponseCancel -> return Nothing
                   ResponseOk     -> fmap Just $ getValue input
    widgetDestroy dialog
    return result
