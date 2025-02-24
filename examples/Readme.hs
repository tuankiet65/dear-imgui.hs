-- NOTE: If this is file is edited, please also copy and paste it into
-- README.md.

{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Main ( main ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Managed
import DearImGui
import DearImGui.OpenGL2
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.GL
import SDL

main :: IO ()
main = do
  -- Initialize SDL
  initializeAll

  runManaged do
    -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
    window <- do
      let title = "Hello, Dear ImGui!"
      let config = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }
      managed $ bracket (createWindow title config) destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext

    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown

    liftIO $ mainLoop window


mainLoop :: Window -> IO ()
mainLoop window = unlessQuit do
  -- Tell ImGui we're starting a new frame
  openGL2NewFrame
  sdl2NewFrame
  newFrame

  -- Build the GUI
  withWindowOpen "Hello, ImGui!" do
    -- Add a text widget
    text "Hello, ImGui!"

    -- Add a button widget, and call 'putStrLn' when it's clicked
    button "Clickety Click" >>= \case
      False -> return ()
      True  -> putStrLn "Ow!"

  -- Show the ImGui demo window
  showDemoWindow

  -- Render
  glClear GL_COLOR_BUFFER_BIT

  render
  openGL2RenderDrawData =<< getDrawData

  glSwapWindow window

  mainLoop window

  where
    -- Process the event loop
    unlessQuit action = do
      shouldQuit <- checkEvents
      if shouldQuit then pure () else action

    checkEvents = do
      pollEventWithImGui >>= \case
        Nothing ->
          return False
        Just event ->
          (isQuit event ||) <$> checkEvents

    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent
