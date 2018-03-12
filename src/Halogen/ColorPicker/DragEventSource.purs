module Halogen.ColorPicker.DragEventSource
  ( dragEventSource
  , DragData
  , CursorEvent
  , DragEvent(..)
  , DragEffects
  , Position
  , mkFirstDragData
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Classy.Event (target)
import DOM.Classy.HTMLElement (getBoundingClientRect)
import DOM.HTML.HTMLElement (DOMRect)
import DOM.Classy.Node (fromNode)
import DOM.Event.EventTarget (EventListener, eventListener, addEventListener, removeEventListener)
import DOM.Event.MouseEvent as MouseE
import DOM.Event.TouchEvent as TouchE
import DOM.Event.Types (Event, EventType)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (mousemove, mouseup, touchend, touchmove)
import DOM.HTML.Types (HTMLElement, windowToEventTarget)
import DOM.HTML.Window (scrollX, scrollY)
import DOM.Node.Types (Node)
import Data.Either (Either(..), either, hush)
import Data.Foldable as F
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen.Query.EventSource as ES

type DragData =
  { page ∷ Position
  , progress ∷ Position
  , pointer ∷ Position
  , delta ∷ Position
  , offset ∷ Position
  }

type CursorEvent = Either MouseE.MouseEvent TouchE.TouchEvent

data DragEvent
  = Move CursorEvent DragData
  | Done CursorEvent

type DragEffects eff =
  ( dom ∷ DOM
  , ref ∷ REF
  , avar ∷ AVAR
  | eff
  )

type Position =
  { x ∷ Number
  , y ∷ Number
  }

dragEventSource
  ∷ ∀ f m eff
  . MonadAff (DragEffects eff) m
  ⇒ CursorEvent
  → (DragEvent → Maybe (f ES.SubscribeStatus))
  → ES.EventSource f m
dragEventSource cursorEvent = ES.eventSource' \emit → do
  posRef ← newRef initPos
  let
    removeListeners ∷ Eff (DragEffects eff) Unit
    removeListeners = do
      win ← windowToEventTarget <$> window
      removeEventListener (cursorMoveFromEvent cursorEvent) cursorMove false win
      removeEventListener (cursorUpFromEvent cursorEvent) cursorUp false win

    cursorMove ∷ EventListener (DragEffects eff)
    cursorMove = eventListener $ onCursorEvent \event → do
      prevPos ← readRef posRef
      dragData ← mkDragData {prev:prevPos, init: initPos} event node
      writeRef posRef dragData.page
      emit $ Move event dragData

    cursorUp ∷ EventListener (DragEffects eff)
    cursorUp = eventListener $ onCursorEvent \event → do
      removeListeners
      emit $ Done event

  win ← windowToEventTarget <$> window
  addEventListener (cursorMoveFromEvent cursorEvent) cursorMove false win
  addEventListener (cursorUpFromEvent cursorEvent) cursorUp false win
  pure removeListeners
  where
  node = cursorEventToTarget cursorEvent
  initPos = cursorEventToPosition cursorEvent



cursorUpFromEvent ∷ CursorEvent → EventType
cursorUpFromEvent = either (const mouseup) (const touchend)

cursorMoveFromEvent ∷ CursorEvent → EventType
cursorMoveFromEvent = either (const mousemove) (const touchmove)

onCursorEvent ∷ ∀ m. Applicative m => (CursorEvent → m Unit) → (Event → m Unit)
onCursorEvent f event = F.traverse_ f $
  map Left asMouseEvent <|> map Right asTouchEvent
  where
  asMouseEvent = hush $ runExcept $ MouseE.eventToMouseEvent event
  asTouchEvent = hush $ runExcept $ TouchE.eventToTouchEvent event

cursorEventToTarget ∷ CursorEvent → Node
cursorEventToTarget = either target target

cursorEventToPosition ∷ CursorEvent → Position
cursorEventToPosition = case _ of
  Left e →
    { x: Int.toNumber $ MouseE.pageX e
    , y: Int.toNumber $ MouseE.pageY e
    }
  Right e → case TouchE.item 0 $ TouchE.touches e of
    Nothing → positionZero
    Just t →
      { x: Int.toNumber $ TouchE.pageX t
      , y: Int.toNumber $ TouchE.pageY t
      }

scrollPosition ∷ ∀ r. Eff (dom ∷ DOM | r) Position
scrollPosition = do
  w ← window
  x ← Int.toNumber <$> scrollX w
  y ← Int.toNumber <$> scrollY w
  pure {x, y}

absoluteDomRect
  ∷ DOMRect
  → Position
  → DOMRect
absoluteDomRect rect scrollPos = rect
  { left = rect.left + scrollPos.x
  , right = rect.right + scrollPos.x
  , top = rect.top + scrollPos.y
  , bottom = rect.bottom + scrollPos.y
  }

nodeBoundingClientRect ∷ ∀ r. Node → Eff (dom ∷ DOM | r) DOMRect
nodeBoundingClientRect node =
  fromMaybe (pure emptyRectangle) $ map getBoundingClientRect elem
  where
  emptyRectangle =
    { left: 0.0
    , right: 0.0
    , top: 0.0
    , bottom: 0.0
    , width: 0.0
    , height: 0.0
    }
  elem ∷ Maybe HTMLElement
  elem = fromNode node

clapInRect ∷ Position → DOMRect → Position
clapInRect { x, y } { left, right, top, bottom } =
  { x: clamp left right x
  , y: clamp top bottom y
  }

positionInRect ∷ Position → DOMRect → Position
positionInRect { x, y } { left, right, top, bottom } =
  { x: x - left
  , y: y - top
  }

progressInRect ∷ Position → DOMRect → Position
progressInRect { x, y } { left, width, top, height } =
  { x: x / width
  , y: y / height
  }

mkDragData
  ∷ ∀ r
  . { prev ∷ Position , init ∷ Position }
  → CursorEvent
  → Node
  → Eff ( dom ∷ DOM | r ) DragData
mkDragData pos event node = do
  rect ← absoluteDomRect <$> nodeBoundingClientRect node <*> scrollPosition
  let
    pagePos = cursorEventToPosition event `clapInRect` rect
    pointer = pagePos `positionInRect` rect
    progress = pointer `progressInRect` rect
  pure
    { page: pagePos
    , pointer -- in rect
    , progress
    , delta: { x: pagePos.x - pos.prev.x, y: pagePos.y - pos.prev.y }
    , offset: { x: pagePos.x - pos.init.x, y : pagePos.y - pos.init.y }
    }

positionZero ∷ Position
positionZero = { x: 0.0, y: 0.0 }

mkFirstDragData ∷ ∀ r. CursorEvent → Eff ( dom ∷ DOM | r ) DragData
mkFirstDragData event = do
  let position = cursorEventToPosition event
  let node = cursorEventToTarget event
  mkDragData { prev: position , init: position } event node
