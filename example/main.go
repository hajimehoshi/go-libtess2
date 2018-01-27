// +build example

package main

import (
	"fmt"
	"image/color"

	"github.com/hajimehoshi/go-libtess2"

	"github.com/hajimehoshi/ebiten"
	"github.com/hajimehoshi/ebiten/ebitenutil"
)

const (
	screenWidth  = 640
	screenHeight = 480
)

func update(screen *ebiten.Image) error {
	e, v, err := libtess2.Tesselate([]libtess2.Contour{
		{
			{X: 0.0, Y: 3.0},
			{X: -1.0, Y: 0.0},
			{X: 1.6, Y: 1.9},
			{X: -1.6, Y: 1.9},
			{X: 1.0, Y: 0.0},
		},
	}, libtess2.WindingRuleOdd)
	if err != nil {
		return err
	}

	if ebiten.IsRunningSlowly() {
		return nil
	}

	for i := 0; i < len(e)/3; i++ {
		for j := 0; j < 3; j++ {
			idx0 := 3*i + j
			idx1 := 3*i + (j+1)%3
			x0 := float64(v[e[idx0]].X*100 + screenWidth/2)
			y0 := float64(v[e[idx0]].Y*100 + screenHeight/2)
			x1 := float64(v[e[idx1]].X*100 + screenWidth/2)
			y1 := float64(v[e[idx1]].Y*100 + screenHeight/2)
			ebitenutil.DrawLine(screen, x0, y0, x1, y1, color.White)
		}
	}

	ebitenutil.DebugPrint(screen, fmt.Sprintf("FPS: %0.2f", ebiten.CurrentFPS()))

	return nil
}

func main() {
	if err := ebiten.Run(update, 640, 480, 1, "go-libtess test"); err != nil {
		panic(err)
	}
}
