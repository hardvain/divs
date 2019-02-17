module DOM.Events where

import DOM.VirtualDOM
import Web.Event.Internal.Types (Event)

onAbort ∷ forall msg. (Event -> msg) -> EventListener msg
onAbort = On "abort"

onBlur ∷ forall msg. (Event -> msg) -> EventListener msg
onBlur = On "blur"

onChange ∷ forall msg. (Event -> msg) -> EventListener msg
onChange = On "change"

onClick ∷ forall msg. (Event -> msg) -> EventListener msg
onClick = On "click"

onContextMenu ∷ forall msg. (Event -> msg) -> EventListener msg
onContextMenu = On "contextmenu"

onDoubleClick ∷ forall msg. (Event -> msg) -> EventListener msg
onDoubleClick = On "dblclick"

onDrag ∷ forall msg. (Event -> msg) -> EventListener msg
onDrag = On "drag"

onDragEnd ∷ forall msg. (Event -> msg) -> EventListener msg
onDragEnd = On "dragend"

onDragEnter ∷ forall msg. (Event -> msg) -> EventListener msg
onDragEnter = On "dragenter"

onDragExit ∷ forall msg. (Event -> msg) -> EventListener msg
onDragExit = On "dragexit"

onDragLeave ∷ forall msg. (Event -> msg) -> EventListener msg
onDragLeave = On "dragleave"

onDragOver ∷ forall msg. (Event -> msg) -> EventListener msg
onDragOver = On "dragover"

onDragStart ∷ forall msg. (Event -> msg) -> EventListener msg
onDragStart = On "dragstart"

onDrop ∷ forall msg. (Event -> msg) -> EventListener msg
onDrop = On "drop"

onError ∷ forall msg. (Event -> msg) -> EventListener msg
onError = On "error"

onFocus ∷ forall msg. (Event -> msg) -> EventListener msg
onFocus = On "focus"

onFocusIn ∷ forall msg. (Event -> msg) -> EventListener msg
onFocusIn = On "focusin"

onFocusOut ∷ forall msg. (Event -> msg) -> EventListener msg
onFocusOut = On "focusout"

onInput ∷ forall msg. (Event -> msg) -> EventListener msg
onInput = On "input"

onInvalid ∷ forall msg. (Event -> msg) -> EventListener msg
onInvalid = On "invalid"

onKeyDown ∷ forall msg. (Event -> msg) -> EventListener msg
onKeyDown = On "keydown"

onKeyPress ∷ forall msg. (Event -> msg) -> EventListener msg
onKeyPress = On "keypress"

onKeyUp ∷ forall msg. (Event -> msg) -> EventListener msg
onKeyUp = On "keyup"

onLoad ∷ forall msg. (Event -> msg) -> EventListener msg
onLoad = On "load"

onMouseDown ∷ forall msg. (Event -> msg) -> EventListener msg
onMouseDown = On "mousedown"

onMouseEnter ∷ forall msg. (Event -> msg) -> EventListener msg
onMouseEnter = On "mouseenter"

onMouseLeave ∷ forall msg. (Event -> msg) -> EventListener msg
onMouseLeave = On "mouseleave"

onMouseMove ∷ forall msg. (Event -> msg) -> EventListener msg
onMouseMove = On "mousemove"

onMouseOver ∷ forall msg. (Event -> msg) -> EventListener msg
onMouseOver = On "mouseover"

onMouseOut ∷ forall msg. (Event -> msg) -> EventListener msg
onMouseOut = On "mouseout"

onMouseUp ∷ forall msg. (Event -> msg) -> EventListener msg
onMouseUp = On "mouseup"

onReset ∷ forall msg. (Event -> msg) -> EventListener msg
onReset = On "reset"

onScroll ∷ forall msg. (Event -> msg) -> EventListener msg
onScroll = On "scroll"

onSelect ∷ forall msg. (Event -> msg) -> EventListener msg
onSelect = On "select"

onSubmit ∷ forall msg. (Event -> msg) -> EventListener msg
onSubmit = On "submit"

onTransitionEnd ∷ forall msg. (Event -> msg) -> EventListener msg
onTransitionEnd = On "transitionend"