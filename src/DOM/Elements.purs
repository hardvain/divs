module DOM.Elements where

import DOM.VirtualDOM
import Data.Array
import Data.Either
import Data.Foldable
import Data.Tuple
import Prelude

import App (Attribute(..), Html(..), Component, EventListener(..), Props)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Web.Event.EventTarget (eventListener)



type PropertyTuple = Tuple String String
type PropertyTuples = Array PropertyTuple
type EventListeners msg = Array (EventListener msg)

node ∷ forall msg. String -> Array (Attribute msg) ->  Array (Html msg) -> Html msg
node nodeName attributes children = mapAttributes attributes html
    where
      html = h nodeName Map.empty children

mapAttributes :: forall msg.  Array (Attribute msg) -> Html msg -> Html msg
mapAttributes attributes html = foldr f html attributes
    where 
        f attribute (Element e) = case mapAttribute attribute of
                                    Left tuple -> Element {name: e.name, children: e.children, listeners:e.listeners,props: Map.insert (fst tuple) (snd tuple) e.props}
                                    Right eventListener -> Element {name: e.name, children: e.children, props: e.props, listeners: eventListener:e.listeners}
        f attribute t = t

mapAttribute :: forall msg. Attribute msg -> Either PropertyTuple (EventListener msg)
mapAttribute (PropertyAttribute k v) = Left $ Tuple k v
mapAttribute (EventListenerAttribute event handler) = Right $ On event handler

div ∷ forall msg. Array (Attribute msg) ->  Array (Html msg) -> Html msg
div = node "div"

button ∷ forall msg. Array (Attribute msg) ->  Array (Html msg) -> Html msg
button = node "button"

code ∷ forall msg. Array (Attribute msg) ->  Array (Html msg) -> Html msg
code = node "code"

h1 ∷ forall msg. Array (Attribute msg) ->  Array (Html msg) -> Html msg
h1 = node "h1"

h1_ :: forall msg. Array (Html msg) -> Html msg
h1_ = h "h1" Map.empty

li ∷ forall msg. Array (Attribute msg) ->  Array (Html msg) -> Html msg
li = node "li"

ul ∷ forall msg. Array (Attribute msg) ->  Array (Html msg) -> Html msg
ul = node "ul"

a ∷ forall msg. Html msg
a = h "a" (Map.empty) []

abbr ∷ forall msg. Html msg
abbr = h "abbr" (Map.empty) []

address ∷ forall msg. Html msg
address = h "address" (Map.empty) []

area ∷ forall msg. Html msg
area = h "area" (Map.empty) []

article ∷ forall msg. Html msg
article = h "article" (Map.empty) []

aside ∷ forall msg. Html msg
aside = h "aside" (Map.empty) []

audio ∷ forall msg. Html msg
audio = h "audio" (Map.empty) []

b ∷ forall msg. Html msg
b = h "b" (Map.empty) []

base ∷ forall msg. Html msg
base = h "base" (Map.empty) []

bdi ∷ forall msg. Html msg
bdi = h "bdi" (Map.empty) []

bdo ∷ forall msg. Html msg
bdo = h "bdo" (Map.empty) []

blockquote ∷ forall msg. Html msg
blockquote = h "blockquote" (Map.empty) []

body ∷ forall msg. Html msg
body = h "body" (Map.empty) []

br ∷ forall msg. Html msg
br = h "br" (Map.empty) []

canvas ∷ forall msg. Html msg
canvas = h "canvas" (Map.empty) []

caption ∷ forall msg. Html msg
caption = h "caption" (Map.empty) []

cite ∷ forall msg. Html msg
cite = h "cite" (Map.empty) []


col ∷ forall msg. Html msg
col = h "col" (Map.empty) []

colgroup ∷ forall msg. Html msg
colgroup = h "colgroup" (Map.empty) []

command ∷ forall msg. Html msg
command = h "command" (Map.empty) []

datalist ∷ forall msg. Html msg
datalist = h "datalist" (Map.empty) []

dd ∷ forall msg. Html msg
dd = h "dd" (Map.empty) []

del ∷ forall msg. Html msg
del = h "del" (Map.empty) []

details ∷ forall msg. Html msg
details = h "details" (Map.empty) []

dfn ∷ forall msg. Html msg
dfn = h "dfn" (Map.empty) []

dialog ∷ forall msg. Html msg
dialog = h "dialog" (Map.empty) []


dl ∷ forall msg. Html msg
dl = h "dl" (Map.empty) []

dt ∷ forall msg. Html msg
dt = h "dt" (Map.empty) []

em ∷ forall msg. Html msg
em = h "em" (Map.empty) []

embed ∷ forall msg. Html msg
embed = h "embed" (Map.empty) []

fieldset ∷ forall msg. Html msg
fieldset = h "fieldset" (Map.empty) []

figcaption ∷ forall msg. Html msg
figcaption = h "figcaption" (Map.empty) []

figure ∷ forall msg. Html msg
figure = h "figure" (Map.empty) []

footer ∷ forall msg. Html msg
footer = h "footer" (Map.empty) []

form ∷ forall msg. Html msg
form = h "form" (Map.empty) []



h2 ∷ forall msg. Html msg
h2 = h "h2" (Map.empty) []

h3 ∷ forall msg. Html msg
h3 = h "h3" (Map.empty) []

h4 ∷ forall msg. Html msg
h4 = h "h4" (Map.empty) []

h5 ∷ forall msg. Html msg
h5 = h "h5" (Map.empty) []

h6 ∷ forall msg. Html msg
h6 = h "h6" (Map.empty) []

head ∷ forall msg. Html msg
head = h "head" (Map.empty) []

header ∷ forall msg. Html msg
header = h "header" (Map.empty) []

hr ∷ forall msg. Html msg
hr = h "hr" (Map.empty) []

html ∷ forall msg. Html msg
html = h "html" (Map.empty) []

i ∷ forall msg. Html msg
i = h "i" (Map.empty) []

iframe ∷ forall msg. Html msg
iframe = h "iframe" (Map.empty) []

img ∷ forall msg. Html msg
img = h "img"  (Map.empty) []

input ∷ forall msg. Html msg
input = h "input" (Map.empty) []

ins ∷ forall msg. Html msg
ins = h "ins" (Map.empty) []

kbd ∷ forall msg. Html msg
kbd = h "kbd" (Map.empty) []

label ∷ forall msg. Html msg
label = h "label" (Map.empty) []



link ∷ forall msg. Html msg
link= h "link"  (Map.empty) []

main ∷ forall msg. Html msg
main = h "main" (Map.empty) []

map ∷ forall msg. Html msg
map = h "map" (Map.empty) []

mark ∷ forall msg. Html msg
mark = h "mark" (Map.empty) []

menu ∷ forall msg. Html msg
menu = h "menu" (Map.empty) []

menuitem ∷ forall msg. Html msg
menuitem = h "menuitem" (Map.empty) []

meta ∷ forall msg. Html msg
meta = h "meta"  (Map.empty) []

meter ∷ forall msg. Html msg
meter = h "meter" (Map.empty) []

nav ∷ forall msg. Html msg
nav = h "nav" (Map.empty) []

noscript ∷ forall msg. Html msg
noscript = h "noscript" (Map.empty) []

object ∷ forall msg. Html msg
object = h "object" (Map.empty) []

ol ∷ forall msg. Html msg
ol = h "ol" (Map.empty) []

optgroup ∷ forall msg. Html msg
optgroup = h "optgroup" (Map.empty) []

option ∷ forall msg. Html msg
option = h "option" (Map.empty) []

output ∷ forall msg. Html msg
output = h "output" (Map.empty) []

p ∷ forall msg. Html msg
p = h "p" (Map.empty) []

param ∷ forall msg. Html msg
param = h "param" (Map.empty) []

pre ∷ forall msg. Html msg
pre = h "pre" (Map.empty) []

progress ∷ forall msg. Html msg
progress = h "progress" (Map.empty) []

q ∷ forall msg. Html msg
q = h "q" (Map.empty) []

rp ∷ forall msg. Html msg
rp = h "rp" (Map.empty) []

rt ∷ forall msg. Html msg
rt = h "rt" (Map.empty) []

ruby ∷ forall msg. Html msg
ruby = h "ruby" (Map.empty) []

samp ∷ forall msg. Html msg
samp = h "samp" (Map.empty) []

script ∷ forall msg. Html msg
script = h "script" (Map.empty) []

section ∷ forall msg. Html msg
section = h "section" (Map.empty) []

select ∷ forall msg. Html msg
select = h "select" (Map.empty) []

small ∷ forall msg. Html msg
small = h "small" (Map.empty) []

source ∷ forall msg. Html msg
source = h "source" (Map.empty) []

span ∷ forall msg. Html msg
span = h "span" (Map.empty) []

strong ∷ forall msg. Html msg
strong = h "strong" (Map.empty) []

style ∷ forall msg. Html msg
style = h "style" (Map.empty) []

sub ∷ forall msg. Html msg
sub = h "sub" (Map.empty) []

summary ∷ forall msg. Html msg
summary = h "summary" (Map.empty) []

sup ∷ forall msg. Html msg
sup = h "sup" (Map.empty) []

table ∷ forall msg. Html msg
table = h "table" (Map.empty) []

tbody ∷ forall msg. Html msg
tbody = h "tbody" (Map.empty) []

td ∷ forall msg. Html msg
td = h "td" (Map.empty) []

textarea ∷ forall msg. Html msg
textarea = h "textarea" (Map.empty) []

tfoot ∷ forall msg. Html msg
tfoot = h "tfoot" (Map.empty) []

th ∷ forall msg. Html msg
th = h "th" (Map.empty) []

thead ∷ forall msg. Html msg
thead = h "thead" (Map.empty) []

time ∷ forall msg. Html msg
time = h "time" (Map.empty) []

title ∷ forall msg. Html msg
title = h "title" (Map.empty) []

tr ∷ forall msg. Html msg
tr = h "tr" (Map.empty) []

u ∷ forall msg. Html msg
u = h "u" (Map.empty) []


var ∷ forall msg. Html msg
var = h "var" (Map.empty) []

video ∷ forall msg. Html msg
video = h "video" (Map.empty) []