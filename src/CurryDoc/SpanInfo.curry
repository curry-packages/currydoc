module CurryDoc.SpanInfo where

import CurryDoc.Span

data SpanInfo = SpanInfo Span [Span]
              | NoSpanInfo
  deriving (Eq, Show, Read)

class HasSpanInfo a where
  getSpanInfo :: a -> SpanInfo

instance HasSpanInfo SpanInfo where
  getSpanInfo = id

getSrcSpan :: HasSpanInfo a => a -> Span
getSrcSpan a = case getSpanInfo a of
  NoSpanInfo   -> NoSpan
  SpanInfo s _ -> s
