/*----------------------------------------------------------------------------
   Copyright (C) Microsoft Corporation
  
   Licensed under the Apache License, Version 2.0 ("The Licence"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the file "license.txt" at the root of this distribution.
----------------------------------------------------------------------------*/

// Basic types of the HTML5 DOM model
public module sys/dom/types

/* Effect primitives */
type hdom :: H
alias dom  = <exn,ndet,console,net,ui,st<hdom>>

/* Type aliases */
type date
alias timestamp = date
alias abstractView = any
alias domTimeStamp = date
alias float32 = double
alias float64 = double
alias int16 = int
alias int32 = int
alias int8 = int
alias milliseconds = int
alias uint16 = int
alias uint32 = int
alias uint8 = int
alias windowTimers = window

/* Enum declarations */
type applicationCacheStatus {
  UNCACHED
  IDLE
  CHECKING
  DOWNLOADING
  UPDATEREADY
  OBSOLETE
}
type cssPrimitiveType {
  CSS_UNKNOWN
  CSS_NUMBER
  CSS_PERCENTAGE
  CSS_EMS
  CSS_EXS
  CSS_PX
  CSS_CM
  CSS_MM
  CSS_IN
  CSS_PT
  CSS_PC
  CSS_DEG
  CSS_RAD
  CSS_GRAD
  CSS_MS
  CSS_S
  CSS_HZ
  CSS_KHZ
  CSS_DIMENSION
  CSS_STRING
  CSS_URI
  CSS_IDENT
  CSS_ATTR
  CSS_COUNTER
  CSS_RECT
  CSS_RGBCOLOR
  CSS_UNICODE_RANGE
}
type cssRuleType {
  STYLE_RULE
  IMPORT_RULE
  MEDIA_RULE
  FONT_FACE_RULE
  PAGE_RULE
  NAMESPACE_RULE
}
type cssValueType {
  CSS_INHERIT
  CSS_PRIMITIVE_VALUE
  CSS_VALUE_LIST
  CSS_CUSTOM
}
type documentPosition {
  DOCUMENT_POSITION_DISCONNECTED
  DOCUMENT_POSITION_PRECEDING
  DOCUMENT_POSITION_FOLLOWING
  DOCUMENT_POSITION_CONTAINS
  DOCUMENT_POSITION_CONTAINED_BY
  DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC
}
type domException {
  INDEX_SIZE_ERR
  DOMSTRING_SIZE_ERR
  HIERARCHY_REQUEST_ERR
  WRONG_DOCUMENT_ERR
  INVALID_CHARACTER_ERR
  NO_DATA_ALLOWED_ERR
  NO_MODIFICATION_ALLOWED_ERR
  NOT_FOUND_ERR
  NOT_SUPPORTED_ERR
  INUSE_ATTRIBUTE_ERR
  INVALID_STATE_ERR
  SYNTAX_ERR
  INVALID_MODIFICATION_ERR
  NAMESPACE_ERR
  INVALID_ACCESS_ERR
  VALIDATION_ERR
  TYPE_MISMATCH_ERR
  SECURITY_ERR
  NETWORK_ERR
  ABORT_ERR
  URL_MISMATCH_ERR
  QUOTA_EXCEEDED_ERR
  TIMEOUT_ERR
  INVALID_NODE_TYPE_ERR
  DATA_CLONE_ERR
}
type eventExceptionCode {
  UNSPECIFIED_EVENT_TYPE_ERR
  DISPATCH_REQUEST_ERR
}
type eventPhase {
  CAPTURING_PHASE
  AT_TARGET
  BUBBLING_PHASE
}
type eventSourceReadyState {
  CONNECTING
  OPEN
  CLOSED
}
type fileErrorCode {
  FILE__NOT_FOUND_ERR
  FILE__SECURITY_ERR
  FILE__ABORT_ERR
  FILE__NOT_READABLE_ERR
  FILE__ENCODING_ERR
}
type fileExceptionCode {
  FILE_NOT_FOUND_ERR
  FILE_SECURITY_ERR
  FILE_ABORT_ERR
  FILE_NOT_READABLE_ERR
  FILE_ENCODING_ERR
  BYTES_PER_ELEMENT
}
type fileReaderReadyState {
  FILE_READER_EMPTY
  FILE_READER_LOADING
  FILE_READER_DONE
}
type keyboardEventLocation {
  DOM_KEY_LOCATION_STANDARD
  DOM_KEY_LOCATION_LEFT
  DOM_KEY_LOCATION_RIGHT
  DOM_KEY_LOCATION_NUMPAD
  DOM_KEY_LOCATION_MOBILE
  DOM_KEY_LOCATION_JOYSTICK
}
type mediaErrorCode {
  MEDIA_ERR_ABORTED
  MEDIA_ERR_NETWORK
  MEDIA_ERR_DECODE
  MEDIA_ERR_SRC_NOT_SUPPORTED
}
type mediaReadyState {
  HAVE_NOTHING
  HAVE_METADATA
  HAVE_CURRENT_DATA
  HAVE_FUTURE_DATA
  HAVE_ENOUGH_DATA
}
type mediaStreamReadyState {
  LIVE
  ENDED
}
type mutationEventAttrChange {
  MODIFICATION
  ADDITION
  REMOVAL
}
type navigatorUserMediaErrorCode {
  PERMISSION_DENIED
}
type networkState {
  NETWORK_EMPTY
  NETWORK_IDLE
  NETWORK_LOADING
  NETWORK_NO_SOURCE
}
type nodeFilterAcceptCode {
  FILTER_ACCEPT
  FILTER_REJECT
  FILTER_SKIP
}
type nodeFilterSelectCode {
  SHOW_ALL
  SHOW_ELEMENT
  SHOW_ATTRIBUTE
  SHOW_TEXT
  SHOW_CDATA_SECTION
  SHOW_ENTITY_REFERENCE
  SHOW_ENTITY
  SHOW_PROCESSING_INSTRUCTION
  SHOW_COMMENT
  SHOW_DOCUMENT
  SHOW_DOCUMENT_TYPE
  SHOW_DOCUMENT_FRAGMENT
  SHOW_NOTATION
}
type nodeType {
  ELEMENT_NODE
  ATTRIBUTE_NODE
  TEXT_NODE
  CDATA_SECTION_NODE
  ENTITY_REFERENCE_NODE
  ENTITY_NODE
  PROCESSING_INSTRUCTION_NODE
  COMMENT_NODE
  DOCUMENT_NODE
  DOCUMENT_TYPE_NODE
  DOCUMENT_FRAGMENT_NODE
  NOTATION_NODE
}
type peerConnectionReadyState {
  PEER_CONNECTION_NEW
  PEER_CONNECTION_NEGOTIATING
  PEER_CONNECTION_ACTIVE
  PEER_CONNECTION_CLOSED
}
type rangeExceptionCode {
  RANGE_BAD_BOUNDARYPOINTS_ERR
  RANGE_INVALID_NODE_TYPE_ERR
}
type rangeExceptionCompareHow {
  START_TO_START
  START_TO_END
  END_TO_END
  END_TO_START
}
type textEventInputMethod {
  DOM_INPUT_METHOD_UNKNOWN
  DOM_INPUT_METHOD_KEYBOARD
  DOM_INPUT_METHOD_PASTE
  DOM_INPUT_METHOD_DROP
  DOM_INPUT_METHOD_IME
  DOM_INPUT_METHOD_OPTION
  DOM_INPUT_METHOD_HANDWRITING
  DOM_INPUT_METHOD_VOICE
  DOM_INPUT_METHOD_MULTIMODAL
  DOM_INPUT_METHOD_SCRIPT
}
type textTrackMode {
  TEXT_TRACK_OFF
  TEXT_TRACK_HIDDEN
  TEXT_TRACK_SHOWING
}
type textTrackReadyState {
  TEXT_TRACK_NONE
  TEXT_TRACK_LOADING
  TEXT_TRACK_LOADED
  TEXT_TRACK_ERROR
}
type wheelEventDeltaMode {
  DOM_DELTA_PIXEL
  DOM_DELTA_LINE
  DOM_DELTA_PAGE
}
type xmlHttpRequestReadyState {
  UNSENT
  OPENED
  HEADERS_RECEIVED
  LOADING
  DONE
}

/* Abstract type declarations for interfaces */
type styleSheetList
external inline get : (this: styleSheetList, index: int) -> dom cssStyleSheet
{ inline "(#1)[#2]" }
external inline set : (this: styleSheetList, index: int, assigned: cssStyleSheet) -> dom ()
{ inline "((#1)[#2] = #3)" }
external inline length : (this: styleSheetList) -> dom int
{ inline "(#1).length" }
type windowBase64
type windowProxy
type seqence // used in `initMessageEvent` and `postMessage`
type anonXmlHttpRequest
type applicationCache
type arrayBuffer
type arrayBufferView
type attr
type audioTrack
type audioTrackList
type barProp
type beforeUnloadEvent
type blob
type blobCallback
type booleanArray
type byteArray
type css2Properties
type cssCharsetRule
type cssColorComponentValue
type cssComponentValue
type cssFontFaceRule
type cssIdentifierComponentValue
type cssImportRule
type cssKeywordComponentValue
type cssLengthComponentValue
type cssMapValue
type cssMediaRule
type cssNamespaceRule
type cssPageRule
type cssPercentageComponentValue
type cssPrimitiveValue
type cssPropertyValue
type cssPropertyValueList
type cssRule
type cssStringComponentValue
type cssStyleDeclaration
type cssStyleDeclarationValue
type cssStyleRule
type cssStyleSheet
type cssUrlComponentValue
type cssUnknownRule
type cssValue
type cssValueList
type canvasGradient
type canvasPattern
type canvasPixelArray
type canvasRenderingContext2d
type caretPosition
type characterData
type clientRect
type clientRectList
type comment
type compositionEvent
type counter
type customEvent
type customEventInit
type domElementMap
type domImplementation
type domImplementationCss
type domSettableTokenList
type domStringList
type domStringMap
type domTokenList
type dataTransfer
type dataTransferItem
type dataTransferItemList
type dataView
type document
type documentCss
type documentFragment
type documentRange
type documentTraversal
type documentType
type doubleArray
type dragEvent
type element
type elementCssInlineStyle
type event
type eventInit
type eventListener
type eventSource
type eventTarget
type external_
type file
type fileCallback
type fileError
type fileList
type fileReader
type fileReaderSync
type float32Array
type float64Array
type floatArray
type focusEvent
type formData
type functionStringCallback
type htmlAllCollection
type htmlAnchorElement
type htmlAppletElement
type htmlAreaElement
type htmlAudioElement
type htmlBrElement
type htmlBaseElement
type htmlBaseFontElement
type htmlBodyElement
type htmlButtonElement
type htmlCanvasElement
type htmlCollection
type htmlCommandElement
type htmlDListElement
type htmlDataListElement
type htmlDetailsElement
type htmlDirectoryElement
type htmlDivElement
type htmlDocument
type htmlElement
type htmlEmbedElement
type htmlFieldSetElement
type htmlFontElement
type htmlFormControlsCollection
type htmlFormElement
type htmlFrameElement
type htmlFrameSetElement
type htmlHrElement
type htmlHeadElement
type htmlHeadingElement
type htmlHtmlElement
type htmlIframeElement
type htmlImageElement
type htmlInputElement
type htmlKeygenElement
type htmlLiElement
type htmlLabelElement
type htmlLegendElement
type htmlLinkElement
type htmlMapElement
type htmlMarqueeElement
type htmlMediaElement
type htmlMenuElement
type htmlMetaElement
type htmlMeterElement
type htmlModElement
type htmlOListElement
type htmlObjectElement
type htmlOptGroupElement
type htmlOptionElement
type htmlOptionsCollection
type htmlOutputElement
type htmlParagraphElement
type htmlParamElement
type htmlPreElement
type htmlProgressElement
type htmlPropertiesCollection
type htmlQuoteElement
type htmlScriptElement
type htmlSelectElement
type htmlSourceElement
type htmlSpanElement
type htmlStyleElement
type htmlTableCaptionElement
type htmlTableCellElement
type htmlTableColElement
type htmlTableDataCellElement
type htmlTableElement
type htmlTableHeaderCellElement
type htmlTableRowElement
type htmlTableSectionElement
type htmlTextAreaElement
type htmlTimeElement
type htmlTitleElement
type htmlTrackElement
type htmlUListElement
type htmlUnknownElement
type htmlVideoElement
type hashChangeEvent
type history
type imageData
type int16Array
type int32Array
type int8Array
type keyboardEvent
type linkStyle
type localMediaStream
type location
type longArray
type longLongArray
type mediaController
type mediaError
type mediaList
type mediaQueryList
type mediaQueryListListener
type mediaStream
type mediaStreamRecorder
type messageChannel
type messageEvent
type messagePort
type mouseEvent
type mutableTextTrack
type mutationEvent
type mutationNameEvent
type navigator
type navigatorUserMediaError
type navigatorUserMediaErrorCallback
type navigatorUserMediaSuccessCallback
type node
type nodeFilter
type nodeIterator
type nodeList
type octetArray
type pageTransitionEvent
type peerConnection
type popStateEvent
type processingInstruction
type progressEvent
type propertyNodeList
type rgbColor
type radioNodeList
type range
type rect
type screen
type shortArray
type signalingCallback
type streamEvent
type streamTrack
type styleSheet
type text
type textEvent
type textMetrics
type textTrack
type textTrackCue
type textTrackCueList
type timeRanges
type transferable
type treeWalker
type uiEvent
type uint16Array
type uint32Array
type uint8Array
type undoManager
type undoManagerEvent
type unsignedByteArray
type unsignedLongArray
type unsignedLongLongArray
type unsignedShortArray
type validityState
type videoTrack
type videoTrackList
type wheelEvent
type window
type windowModal
type xmlHttpRequest
type xmlHttpRequestEventTarget
type xmlHttpRequestUpload
