+++
title = "Color Experiments"
author = "Tony Day"
lastmod = 2022-05-28
draft = false
tags = ["color"]
+++

This is a snapshot of some ongoing colour R&D, mostly for the [chart-svg](https://hackage.haskell.org/package/chart-svg) library. The development repo can be found at [GitHub - tonyday567/color-adjust](https://github.com/tonyday567/color-adjust). The readme.org there is runnable and contains the code used to create the examples below.

## oklch {#oklch}

A new color space model, [oklab](https://bottosson.github.io/posts/oklab/), has been introduced to the [CSS standards](https://www.w3.org/TR/css-color-4/#rgb-functions).

The basic idea is that this color space is _ok_ with respect to human perception of graduated changes in colour. The LCH version of oklab consists of three element:

-   [lightness](https://en.wikipedia.org/wiki/Lightness), from 0 (black) to 1 (white)
-   [chromacity](https://en.wikipedia.org/wiki/Chromaticity), from 0 (grey) to a point at which the colour goes out of RGB gamut (it varies but typically 0.25 to 0.33)
-   [hue](https://en.wikipedia.org/wiki/Hue), from 0 to 360, representing colour as a rainbow circle in degrees.


## LCHA {#lcha}

Adding in an [alpha](https://en.wikipedia.org/wiki/Alpha%5Fcompositing) channel, which is a graduated model of colour opacity, we create a 4 element type, LCHA:

| Channel | Range       | Description |
|---|---|---|
|---------|-------------|-------------|
| l       | [0, 1]      | Lightness   |
| c       | [0, 0.322]≈ | Chroma      |
| h       | [0, 360)    | Hue         |
| a       | [0, 1]      | Alpha       |
We take some colours from the chart-svg palette and convert them to LCHA:

```text

[Colour 0.02 0.73 0.80 1.00,Colour 0.02 0.29 0.48 1.00,Colour 0.66 0.07 0.55 1.00,Colour 0.96 0.60 0.92 1.00]
```

```text
> > > LCHA 0.720 0.123 207 1.000
LCHA 0.400 0.100 246 1.000
LCHA 0.500 0.210 338 1.000
LCHA 0.800 0.150 331 1.000
```


## Decomposition {#decomposition}

The swatches below decompose these colours into LCH components, by setting each component to constant values:


<div>

<div class=swatch style="background:rgba(2%, 73%, 80%, 1.00);"></div>
<div class=swatch style="background:rgba(2%, 29%, 48%, 1.00);"></div>
<div class=swatch style="background:rgba(66%, 7%, 55%, 1.00);"></div>
<div class=swatch style="background:rgba(96%, 60%, 92%, 1.00);"></div>
original
</div>
<div>

<div class=swatch style="background:rgba(64%, 64%, 64%, 1.00);"></div>
<div class=swatch style="background:rgba(28%, 28%, 28%, 1.00);"></div>
<div class=swatch style="background:rgba(39%, 39%, 39%, 1.00);"></div>
<div class=swatch style="background:rgba(74%, 74%, 74%, 1.00);"></div>
zero chroma
</div>
<div>

<div class=swatch style="background:rgba(-63%, 46%, 53%, 1.00);"></div>
<div class=swatch style="background:rgba(17%, 41%, 60%, 1.00);"></div>
<div class=swatch style="background:rgba(66%, 7%, 55%, 1.00);"></div>
<div class=swatch style="background:rgba(57%, 24%, 54%, 1.00);"></div>
constant lightness
</div>
<div>

<div class=swatch style="background:rgba(-110%, 47%, 56%, 1.00);"></div>
<div class=swatch style="background:rgba(-21%, 40%, 70%, 1.00);"></div>
<div class=swatch style="background:rgba(59%, 23%, 50%, 1.00);"></div>
<div class=swatch style="background:rgba(57%, 24%, 54%, 1.00);"></div>
constant lightness & chroma
</div>
<div>

<div class=swatch style="background:rgba(-53%, 47%, 40%, 1.00);"></div>
<div class=swatch style="background:rgba(-15%, 46%, 40%, 1.00);"></div>
<div class=swatch style="background:rgba(-190%, 52%, 41%, 1.00);"></div>
<div class=swatch style="background:rgba(-96%, 49%, 40%, 1.00);"></div>
constant lightness and hue
</div>


## Mapping LCHA {#mapping-lcha}

The chart below plots these 4 colours versus a 2-D slice of the oklab space (a 3-D one) with constant lightness.

{{< figure src="../artifacts/color-adjust/dotwheel.svg" >}}

Note that the light blue color (LCHA 0.720 0.123 207 1.000) is well outside the [gamut](https://en.wikipedia.org/wiki/Gamut) for the wheel lightness chosen. This is a common occurence and the shape of the chroma range varies considerably with both hue and lightness.

```haskell
view lcha2colour' (LCHA 0.6 0.123 207 1.000)
```

```text
Colour -0.50 0.58 0.65 1.00
```

Clipping this result to 0 results in significant hue shift.

Compare the shape of gamuts for lightnesses of 0.3 and 0.7


### lightness=0.3 {#lightness-0-dot-3}

{{< figure src="../artifacts/color-adjust/wheel3.svg" >}}


### lightness=0.7 {#lightness-0-dot-7}

{{< figure src="../artifacts/color-adjust/wheel7.svg" >}}


## Gradient Charts {#gradient-charts}

Mixing of colours resulting in the interpolated colour being out of gamut is the major drawwback of the oklch space. Otherwise, the mixing of colours is quite pleasing from this human's perspective:

```haskell
c = Colour 0.72 0.34 0.04 1
ok = view (re lcha2colour') c
print c
prettyLCHA ok
```

```text

> Colour 0.72 0.34 0.04 1.00
LCHA 0.566 0.146 50 1.000
```

fade to white (LCH 0 0 50 1)

{{< figure src="../artifacts/color-adjust/towhite.svg" >}}

fade to grey (LCH 0.566 0 50 1)

{{< figure src="../artifacts/color-adjust/togrey.svg" >}}

fade to hue=0 (LCH 0.566 0.146 0 1)

{{< figure src="../artifacts/color-adjust/tohue0.svg" >}}

lightness 0 to 1

{{< figure src="../artifacts/color-adjust/lightness.svg" >}}

The red drift is due to being out of gamut.

chroma 0 to 0.25

{{< figure src="../artifacts/color-adjust/chroma.svg" >}}

hue 0 to 360

{{< figure src="../artifacts/color-adjust/hue.svg" >}}

Whatever the relative success of oklab in providing pleasing human perceptions of consistent colour change, inclusion in the CSS standards are likely to provide a leg up to its usage going forward.


## palette1 Testing {#palette1-testing}

Full palette1 mapping for chart-svg.

```haskell
x1 =[LCHA 0.72 0.123 207 1, LCHA 0.40 0.10 246 1, LCHA 0.50 0.21 338 1, LCHA 0.8 0.15 331 1, LCHA 0.83 0.14 69 1, LCHA 0.57 0.15 50 1, LCHA 0.38 0.085 128 1, LCHA 0.60 0.08 104 1] :: [LCHA]
cs = trimColour <$> view lcha2colour' <$> x1
csu = view lcha2colour' <$> x1
print csu
```


<div>

<div class=swatch style="background:rgba(2%, 73%, 80%, 1.00);"></div>
<div class=swatch style="background:rgba(2%, 29%, 48%, 1.00);"></div>
<div class=swatch style="background:rgba(66%, 7%, 55%, 1.00);"></div>
<div class=swatch style="background:rgba(96%, 60%, 92%, 1.00);"></div>
<div class=swatch style="background:rgba(100%, 71%, 35%, 1.00);"></div>
<div class=swatch style="background:rgba(73%, 34%, 1%, 1.00);"></div>
<div class=swatch style="background:rgba(21%, 29%, 7%, 1.00);"></div>
<div class=swatch style="background:rgba(53%, 51%, 28%, 1.00);"></div>
original
</div>
<div>

<div class=swatch style="background:rgba(64%, 64%, 64%, 1.00);"></div>
<div class=swatch style="background:rgba(28%, 28%, 28%, 1.00);"></div>
<div class=swatch style="background:rgba(39%, 39%, 39%, 1.00);"></div>
<div class=swatch style="background:rgba(74%, 74%, 74%, 1.00);"></div>
<div class=swatch style="background:rgba(78%, 78%, 78%, 1.00);"></div>
<div class=swatch style="background:rgba(47%, 47%, 47%, 1.00);"></div>
<div class=swatch style="background:rgba(26%, 26%, 26%, 1.00);"></div>
<div class=swatch style="background:rgba(50%, 50%, 50%, 1.00);"></div>
greyed
</div>
<div>

<div class=swatch style="background:rgba(-63%, 46%, 53%, 1.00);"></div>
<div class=swatch style="background:rgba(17%, 41%, 60%, 1.00);"></div>
<div class=swatch style="background:rgba(66%, 7%, 55%, 1.00);"></div>
<div class=swatch style="background:rgba(57%, 24%, 54%, 1.00);"></div>
<div class=swatch style="background:rgba(58%, 32%, -28%, 1.00);"></div>
<div class=swatch style="background:rgba(64%, 26%, -14%, 1.00);"></div>
<div class=swatch style="background:rgba(33%, 42%, 21%, 1.00);"></div>
<div class=swatch style="background:rgba(42%, 40%, 17%, 1.00);"></div>
lightness 0.5
</div>
<div>

<div class=swatch style="background:rgba(-110%, 47%, 56%, 1.00);"></div>
<div class=swatch style="background:rgba(-21%, 40%, 70%, 1.00);"></div>
<div class=swatch style="background:rgba(59%, 23%, 50%, 1.00);"></div>
<div class=swatch style="background:rgba(57%, 24%, 54%, 1.00);"></div>
<div class=swatch style="background:rgba(59%, 31%, -37%, 1.00);"></div>
<div class=swatch style="background:rgba(64%, 26%, -14%, 1.00);"></div>
<div class=swatch style="background:rgba(29%, 45%, -17%, 1.00);"></div>
<div class=swatch style="background:rgba(45%, 40%, -42%, 1.00);"></div>
l=0.5, chroma=0.15
</div>
<div>

<div class=swatch style="background:rgba(95%, 55%, 55%, 1.00);"></div>
<div class=swatch style="background:rgba(90%, 58%, 58%, 1.00);"></div>
<div class=swatch style="background:rgba(110%, 41%, 46%, 1.00);"></div>
<div class=swatch style="background:rgba(100%, 52%, 53%, 1.00);"></div>
<div class=swatch style="background:rgba(98%, 53%, 54%, 1.00);"></div>
<div class=swatch style="background:rgba(100%, 52%, 53%, 1.00);"></div>
<div class=swatch style="background:rgba(87%, 60%, 59%, 1.00);"></div>
<div class=swatch style="background:rgba(86%, 60%, 60%, 1.00);"></div>
l=0.75, hue=20
</div>

{{< figure src="../artifacts/color-adjust/palette1.svg" >}}


### Line chart {#line-chart}

{{< figure src="../artifacts/color-adjust/scratch.svg" >}}


## paletteR Testing {#paletter-testing}

<kbd>paletteR</kbd> provides random colours, without being too dark or light to use in a chart.

```haskell
cs = (\x -> paletteR !! x) <$> ([0..7] :: [Int])
print cs
```

{{< figure src="../artifacts/color-adjust/paletter.svg" >}}

{{< figure src="../artifacts/color-adjust/liner.svg" >}}


## References {#references}

-   CSS colors are defined as [sRGB - Wikipedia](https://en.wikipedia.org/wiki/SRGB), with D65 illuminate.
-   [A perceptual color space for image processing](https://bottosson.github.io/posts/oklab/)
-   [CSS Color Module Level 5](https://www.w3.org/TR/css-color-5/#colorcontrast)
-   [CSS Color Module Level 4](https://www.w3.org/TR/css-color-4/#rgb-functions)
-   [CSS Color Module Level 3](https://www.w3.org/TR/css-color-3/)
-   [CIELAB color space - Wikipedia](https://en.wikipedia.org/wiki/CIELAB%5Fcolor%5Fspace)
-   <https://observablehq.com/@fil/oklab-color-space>

<style>
.swatch {
  border-radius:20%;
  display: inline-block;
  margin:10px;
  width: 20px;
  height: 20px;
  overflow: hidden;
  font-size: 0px;
}
.swatch:hover {
  margin: 0;
  width: 40px;
  height: 40px;
  line-height: 40px;
  font-size: 6px;
  color: rgb(12 12 12);
  text-align: center;
  overflow: hidden;
}
</style>
