module DOM.Elements where

import DOM.VirtualDOM
import Data.Array
import Data.Either
import Data.Foldable
import Data.Tuple
import Prelude

import App 
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Web.Event.EventTarget (eventListener)

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

div_ ∷ forall msg. Array (Html msg) -> Html msg
div_ = node "div" []

button ∷ forall msg. Array (Attribute msg) ->  Array (Html msg) -> Html msg
button = node "button"

button_ ∷ forall msg. Array (Html msg) -> Html msg
button_ = node "button" []

code ∷ forall msg. Array (Attribute msg) ->  Array (Html msg) -> Html msg
code = node "code"

code_ ∷ forall msg. Array (Html msg) -> Html msg
code_ = node "code" []

h1 ∷ forall msg. Array (Attribute msg) ->  Array (Html msg) -> Html msg
h1 = node "h1"

h1_ ∷ forall msg. Array (Html msg) -> Html msg
h1_ = node "h1" []

li ∷ forall msg. Array (Attribute msg) ->  Array (Html msg) -> Html msg
li = node "li"

li_ ∷ forall msg. Array (Html msg) -> Html msg
li_ = node "li" []

ul ∷ forall msg. Array (Attribute msg) ->  Array (Html msg) -> Html msg
ul = node "ul"

ul_ ∷ forall msg. Array (Html msg) -> Html msg
ul_ = node "ul" []

a ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
a = node "a"

a_ ∷ forall msg. Array (Html msg) -> Html msg
a_ = node "a" []

abbr ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
abbr = node "abbr"

abbr_ ∷ forall msg. Array (Html msg) -> Html msg
abbr_ = node "abbr" []

address ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
address = node "address"

address_ ∷ forall msg. Array (Html msg) -> Html msg
address_ = node "address" []

area ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
area = node "area"

area_ ∷ forall msg. Array (Html msg) -> Html msg
area_ = node "area" []

article ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
article = node "article"

article_ ∷ forall msg. Array (Html msg) -> Html msg
article_ = node "article" []

aside ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
aside = node "aside"

aside_ ∷ forall msg. Array (Html msg) -> Html msg
aside_ = node "aside" []

audio ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
audio = node "audio"

audio_ ∷ forall msg. Array (Html msg) -> Html msg
audio_ = node "audio" []

b ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
b = node "b"

b_ ∷ forall msg. Array (Html msg) -> Html msg
b_ = node "b" []

base ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
base = node "base"

base_ ∷ forall msg. Array (Html msg) -> Html msg
base_ = node "base" []

bdi ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
bdi = node "bdi"

bdi_ ∷ forall msg. Array (Html msg) -> Html msg
bdi_ = node "bdi" []

bdo ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
bdo = node "bdo"

bdo_ ∷ forall msg. Array (Html msg) -> Html msg
bdo_ = node "bdo" []

blockquote ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
blockquote = node "blockquote"

blockquote_ ∷ forall msg. Array (Html msg) -> Html msg
blockquote_ = node "blockquote" []

body ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
body = node "body"

body_ ∷ forall msg. Array (Html msg) -> Html msg
body_ = node "body" []

br ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
br = node "br"

br_ ∷ forall msg. Array (Html msg) -> Html msg
br_ = node "br" []

canvas ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
canvas = node "canvas"

canvas_ ∷ forall msg. Array (Html msg) -> Html msg
canvas_ = node "canvas" []

caption ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
caption = node "caption"

caption_ ∷ forall msg. Array (Html msg) -> Html msg
caption_ = node "caption" []

cite ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
cite = node "cite"

cite_ ∷ forall msg. Array (Html msg) -> Html msg
cite_ = node "cite" []

col ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
col = node "col"

col_ ∷ forall msg. Array (Html msg) -> Html msg
col_ = node "col" []

colgroup ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
colgroup = node "colgroup"

colgroup_ ∷ forall msg. Array (Html msg) -> Html msg
colgroup_ = node "colgroup" []

command ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
command = node "command"

command_ ∷ forall msg. Array (Html msg) -> Html msg
command_ = node "command" []

datalist ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
datalist = node "datalist"

datalist_ ∷ forall msg. Array (Html msg) -> Html msg
datalist_ = node "datalist" []

dd ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
dd = node "dd"

dd_ ∷ forall msg. Array (Html msg) -> Html msg
dd_ = node "dd" []

del ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
del = node "del"

del_ ∷ forall msg. Array (Html msg) -> Html msg
del_ = node "del" []

details ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
details = node "details"

details_ ∷ forall msg. Array (Html msg) -> Html msg
details_ = node "details" []

dfn ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
dfn = node "dfn"

dfn_ ∷ forall msg. Array (Html msg) -> Html msg
dfn_ = node "dfn" []

dialog ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
dialog = node "dialog"

dialog_ ∷ forall msg. Array (Html msg) -> Html msg
dialog_ = node "dialog" []

dl ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
dl = node "dl"

dl_ ∷ forall msg. Array (Html msg) -> Html msg
dl_ = node "dl" []

dt ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
dt = node "dt"

dt_ ∷ forall msg. Array (Html msg) -> Html msg
dt_ = node "dt" []

em ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
em = node "em"

em_ ∷ forall msg. Array (Html msg) -> Html msg
em_ = node "em" []

embed ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
embed = node "embed"

embed_ ∷ forall msg. Array (Html msg) -> Html msg
embed_ = node "embed" []

fieldset ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
fieldset = node "fieldset"

fieldset_ ∷ forall msg. Array (Html msg) -> Html msg
fieldset_ = node "fieldset" []

figcaption ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
figcaption = node "figcaption"

figcaption_ ∷ forall msg. Array (Html msg) -> Html msg
figcaption_ = node "figcaption" []

figure ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
figure = node "figure"

figure_ ∷ forall msg. Array (Html msg) -> Html msg
figure_ = node "figure" []

footer ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
footer = node "footer"

footer_ ∷ forall msg. Array (Html msg) -> Html msg
footer_ = node "footer" []

form ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
form = node "form"

form_ ∷ forall msg. Array (Html msg) -> Html msg
form_ = node "form" []

h2 ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
h2 = node "h2"

h2_ ∷ forall msg. Array (Html msg) -> Html msg
h2_ = node "h2" []

h3 ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
h3 = node "h3"

h3_ ∷ forall msg. Array (Html msg) -> Html msg
h3_ = node "h3" []

h4 ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
h4 = node "h4"

h4_ ∷ forall msg. Array (Html msg) -> Html msg
h4_ = node "h4" []

h5 ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
h5 = node "h5"

h5_ ∷ forall msg. Array (Html msg) -> Html msg
h5_ = node "h5" []

h6 ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
h6 = node "h6"

h6_ ∷ forall msg. Array (Html msg) -> Html msg
h6_ = node "h6" []

head ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
head = node "head"

head_ ∷ forall msg. Array (Html msg) -> Html msg
head_ = node "head" []

header ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
header = node "header"

header_ ∷ forall msg. Array (Html msg) -> Html msg
header_ = node "header" []

hr ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
hr = node "hr"

hr_ ∷ forall msg. Array (Html msg) -> Html msg
hr_ = node "hr" []

html ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
html = node "html"

html_ ∷ forall msg. Array (Html msg) -> Html msg
html_ = node "html" []

i ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
i = node "i"

i_ ∷ forall msg. Array (Html msg) -> Html msg
i_ = node "i" []

iframe ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
iframe = node "iframe"

iframe_ ∷ forall msg. Array (Html msg) -> Html msg
iframe_ = node "iframe" []

img ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
img = node "img"

img_ ∷ forall msg. Array (Html msg) -> Html msg
img_ = node "img" []

input ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
input = node "input"

input_ ∷ forall msg. Array (Html msg) -> Html msg
input_ = node "input" []

ins ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
ins = node "ins"

ins_ ∷ forall msg. Array (Html msg) -> Html msg
ins_ = node "ins" []

kbd ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
kbd = node "kbd"

kbd_ ∷ forall msg. Array (Html msg) -> Html msg
kbd_ = node "kbd" []

label ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
label = node "label"

label_ ∷ forall msg. Array (Html msg) -> Html msg
label_ = node "label" []

link ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
link= node "link"

link_ ∷ forall msg. Array (Html msg) -> Html msg
link_= node "link" []

main ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
main = node "main"

main_ ∷ forall msg. Array (Html msg) -> Html msg
main_ = node "main" []

map ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
map = node "map"

map_ ∷ forall msg. Array (Html msg) -> Html msg
map_ = node "map" []

mark ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
mark = node "mark"

mark_ ∷ forall msg. Array (Html msg) -> Html msg
mark_ = node "mark" []

menu ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
menu = node "menu"

menu_ ∷ forall msg. Array (Html msg) -> Html msg
menu_ = node "menu" []

menuitem ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
menuitem = node "menuitem"

menuitem_ ∷ forall msg. Array (Html msg) -> Html msg
menuitem_ = node "menuitem" []

meta ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
meta = node "meta"

meta_ ∷ forall msg. Array (Html msg) -> Html msg
meta_ = node "meta" []

meter ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
meter = node "meter"

meter_ ∷ forall msg. Array (Html msg) -> Html msg
meter_ = node "meter" []

nav ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
nav = node "nav"

nav_ ∷ forall msg. Array (Html msg) -> Html msg
nav_ = node "nav" []

noscript ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
noscript = node "noscript"

noscript_ ∷ forall msg. Array (Html msg) -> Html msg
noscript_ = node "noscript" []

object ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
object = node "object"

object_ ∷ forall msg. Array (Html msg) -> Html msg
object_ = node "object" []

ol ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
ol = node "ol"

ol_ ∷ forall msg. Array (Html msg) -> Html msg
ol_ = node "ol" []

optgroup ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
optgroup = node "optgroup"

optgroup_ ∷ forall msg. Array (Html msg) -> Html msg
optgroup_ = node "optgroup" []

option ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
option = node "option"

option_ ∷ forall msg. Array (Html msg) -> Html msg
option_ = node "option" []

output ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
output = node "output"

output_ ∷ forall msg. Array (Html msg) -> Html msg
output_ = node "output" []

p ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
p = node "p"

p_ ∷ forall msg. Array (Html msg) -> Html msg
p_ = node "p" []

param ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
param = node "param"

param_ ∷ forall msg. Array (Html msg) -> Html msg
param_ = node "param" []

pre ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
pre = node "pre"

pre_ ∷ forall msg. Array (Html msg) -> Html msg
pre_ = node "pre" []

progress ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
progress = node "progress"

progress_ ∷ forall msg. Array (Html msg) -> Html msg
progress_ = node "progress" []

q ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
q = node "q"

q_ ∷ forall msg. Array (Html msg) -> Html msg
q_ = node "q" []

rp ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
rp = node "rp"

rp_ ∷ forall msg. Array (Html msg) -> Html msg
rp_ = node "rp" []

rt ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
rt = node "rt"

rt_ ∷ forall msg. Array (Html msg) -> Html msg
rt_ = node "rt" []

ruby ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
ruby = node "ruby"

ruby_ ∷ forall msg. Array (Html msg) -> Html msg
ruby_ = node "ruby" []

samp ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
samp = node "samp"

samp_ ∷ forall msg. Array (Html msg) -> Html msg
samp_ = node "samp" []

script ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
script = node "script"

script_ ∷ forall msg. Array (Html msg) -> Html msg
script_ = node "script" []

section ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
section = node "section"

section_ ∷ forall msg. Array (Html msg) -> Html msg
section_ = node "section" []

select ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
select = node "select"

select_ ∷ forall msg. Array (Html msg) -> Html msg
select_ = node "select" []

small ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
small = node "small"

small_ ∷ forall msg. Array (Html msg) -> Html msg
small_ = node "small" []

source ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
source = node "source"

source_ ∷ forall msg. Array (Html msg) -> Html msg
source_ = node "source" []

span ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
span = node "span"

span_ ∷ forall msg. Array (Html msg) -> Html msg
span_ = node "span" []

strong ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
strong = node "strong"

strong_ ∷ forall msg. Array (Html msg) -> Html msg
strong_ = node "strong" []

style ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
style = node "style"

style_ ∷ forall msg. Array (Html msg) -> Html msg
style_ = node "style" []

sub ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
sub = node "sub"

sub_ ∷ forall msg. Array (Html msg) -> Html msg
sub_ = node "sub" []

summary ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
summary = node "summary"

summary_ ∷ forall msg. Array (Html msg) -> Html msg
summary_ = node "summary" []

sup ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
sup = node "sup"

sup_ ∷ forall msg. Array (Html msg) -> Html msg
sup_ = node "sup" []

table ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
table = node "table"

table_ ∷ forall msg. Array (Html msg) -> Html msg
table_ = node "table" []

tbody ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
tbody = node "tbody"

tbody_ ∷ forall msg. Array (Html msg) -> Html msg
tbody_ = node "tbody" []

td ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
td = node "td"

td_ ∷ forall msg. Array (Html msg) -> Html msg
td_ = node "td" []

textarea ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
textarea = node "textarea"

textarea_ ∷ forall msg. Array (Html msg) -> Html msg
textarea_ = node "textarea" []

tfoot ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
tfoot = node "tfoot"

tfoot_ ∷ forall msg. Array (Html msg) -> Html msg
tfoot_ = node "tfoot" []

th ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
th = node "th"

th_ ∷ forall msg. Array (Html msg) -> Html msg
th_ = node "th" []

thead ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
thead = node "thead"

thead_ ∷ forall msg. Array (Html msg) -> Html msg
thead_ = node "thead" []

time ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
time = node "time"

time_ ∷ forall msg. Array (Html msg) -> Html msg
time_ = node "time" []

title ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
title = node "title"

title_ ∷ forall msg. Array (Html msg) -> Html msg
title_ = node "title" []

tr ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
tr = node "tr"

tr_ ∷ forall msg. Array (Html msg) -> Html msg
tr_ = node "tr" []

u ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
u = node "u"

u_ ∷ forall msg. Array (Html msg) -> Html msg
u_ = node "u" []

var ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg
var = node "var"

var_ ∷ forall msg. Array (Html msg) -> Html msg
var_ = node "var" []

video ∷ forall msg. Array (Attribute msg) -> Array (Html msg) -> Html msg 
video = node "video"

video_ ∷ forall msg. Array (Html msg) -> Html msg 
video_ = node "video" []
