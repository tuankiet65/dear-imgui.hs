{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module DearImGui.Raw.MemoryEditor
  ( MemoryEditor
  , new
  , delete
  , setReadOnly
  , setShowDataPreview
  , drawWindow
  )
  where

import Control.Monad.IO.Class
  ( MonadIO, liftIO )
import Foreign hiding (new)
import Foreign.C

-- dear-imgui
import DearImGui.Raw.Context
  ( imguiContext )
import DearImGui.Structs
  ( MemoryEditor )

-- inline-c
import qualified Language.C.Inline as C

-- inline-c-cpp
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx <> imguiContext)
C.include "imgui.h"
C.include "imgui_memory_editor/imgui_memory_editor.h"
Cpp.using "namespace ImGui"

type PtrMemoryEditor = Ptr MemoryEditor

new :: (MonadIO m) => m PtrMemoryEditor
new = liftIO do
  [C.block|
    MemoryEditor* {
      return IM_NEW(MemoryEditor);
    }
  |]

delete :: (MonadIO m) => PtrMemoryEditor -> m ()
delete editor = liftIO do
  [C.block|
    void {
      IM_DELETE($(MemoryEditor* editor));
    }
  |]

setReadOnly :: (MonadIO m) => PtrMemoryEditor -> CBool -> m ()
setReadOnly editor readOnly = liftIO do
  [C.block|
    void {
      $(MemoryEditor* editor)->ReadOnly = $(bool readOnly);
    }
  |]

setShowDataPreview :: (MonadIO m) => PtrMemoryEditor -> CBool -> m ()
setShowDataPreview editor showDataPreview = liftIO do
  [C.block|
    void {
      $(MemoryEditor* editor)->OptShowDataPreview = $(bool showDataPreview);
    }
  |]

drawWindow :: (MonadIO m) => PtrMemoryEditor -> CString -> Ptr () -> CSize -> CSize -> m ()
drawWindow editor title memData memSize baseDisplayAddr = liftIO do
  [C.block|
    void {
      $(MemoryEditor* editor)->DrawWindow(
        $(const char *title),
        $(void* memData),
        $(size_t memSize),
        $(size_t baseDisplayAddr)
      );
    }
  |]