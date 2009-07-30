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

showInfoDialog :: String -> String -> IO ()
showInfoDialog t m = dialogOfType MessageInfo ButtonsOk t m >> return ()

showErrorDialog :: String -> String -> IO ()
showErrorDialog t m = dialogOfType MessageError ButtonsOk t m >> return ()

showYesNoDialog :: String -> String -> IO Bool
showYesNoDialog = dialogOfType MessageQuestion ButtonsYesNo

showOkCancelDialog :: String -> String -> IO Bool
showOkCancelDialog = dialogOfType MessageWarning ButtonsOkCancel

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

titledPrompt title = do
    dialog <- dialogNew
    set dialog [ windowTitle := title ]
    dialogAddButton dialog stockOk ResponseOk
    dialogAddButton dialog stockCancel ResponseCancel

    windowSetTypeHint dialog WindowTypeHintDialog
    windowSetKeepAbove dialog True
    windowSetSkipTaskbarHint dialog True
    windowSetSkipPagerHint dialog True

    dialogSetDefaultResponse dialog ResponseOk

    return dialog

dialogAddWidget dialog widget = do
    box <- dialogGetUpper dialog
    boxPackStart box widget PackGrow 10
    widgetShowAll box

showTextDialog :: String -> String -> IO (Maybe String)
showTextDialog title prompt = do
    initGUI
    dialog <- titledPrompt title
    label <- labelNew $ Just prompt
    input <- entryNew
    dialogAddWidget dialog label
    dialogAddWidget dialog input
    onEntryActivate input $ dialogResponse dialog ResponseOk

    response <- dialogRun dialog
    result <- case response of
                   ResponseCancel -> return Nothing
                   ResponseOk     -> do text <- entryGetText input
                                        return . Just $ text
    widgetDestroy dialog
    return result

showOptionDialog :: String -> String -> [String] -> IO (Maybe String)
showOptionDialog title prompt options = do
    initGUI
    dialog <- titledPrompt title
    label <- labelNew $ Just prompt
    input <- comboBoxNewText
    mapM (comboBoxAppendText input) options
    dialogAddWidget dialog label
    dialogAddWidget dialog input

    response <- dialogRun dialog
    result <- case response of
                   ResponseCancel -> return Nothing
                   ResponseOk     -> do idx <- comboBoxGetActive input
                                        return . Just $ options !! idx
    widgetDestroy dialog
    return result

showPasswordDialog :: String -> String -> IO (Maybe String)
showPasswordDialog title prompt = do
    initGUI
    dialog <- titledPrompt title
    pwBox <- entryNew
    set pwBox [entryVisibility := False]
    onEntryActivate pwBox $ dialogResponse dialog ResponseOk

    label <- labelNew $ Just prompt
    dialogAddWidget dialog label
    dialogAddWidget dialog pwBox

    response <- dialogRun dialog
    result <- case response of
                   ResponseCancel -> return Nothing
                   ResponseOk     -> do text <- entryGetText pwBox
                                        return . Just $ text
    widgetDestroy dialog
    return result
