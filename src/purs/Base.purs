module Base where

foreign import undefined :: forall a. a

foreign import pop
  "function pop(l) {\
  \  if(l.length == 0) return l;\
  \  var l1 = l.slice();\
  \  l1.pop(); \
  \  return l1;\
  \}" :: forall a. [a] -> [a]