package libtess2_test

import (
	"fmt"

	. "github.com/hajimehoshi/go-libtess2"
)

func ExampleTriangles() {
	contour := []float32{
		0.0, 3.0,
		-1.0, 0.0,
		1.6, 1.9,
		-1.6, 1.9,
		1.0, 0.0}
	t := NewTesselator()
	t.AddContour(contour)
	e, v, err := t.Tesselate()
	if err != nil {
		panic(err)
	}
	for i := 0; i < len(e) / 3; i++ {
		fmt.Printf("(%.1f, %.1f), (%.1f, %.1f), (%.1f, %.1f)\n",
			v[e[3*i]].X, v[e[3*i]].Y,
			v[e[3*i+1]].X, v[e[3*i+1]].Y,
			v[e[3*i+2]].X, v[e[3*i+2]].Y)
	}
	// Output:
	// (0.4, 1.9), (0.0, 3.0), (-0.4, 1.9)
	// (1.6, 1.9), (0.4, 1.9), (0.6, 1.2)
	// (1.0, 0.0), (0.6, 1.2), (-0.0, 0.7)
	// (-0.0, 0.7), (-0.6, 1.2), (-1.0, 0.0)
	// (-0.4, 1.9), (-1.6, 1.9), (-0.6, 1.2)
}
