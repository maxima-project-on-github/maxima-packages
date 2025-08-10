package main

import "math"

func foo0 () float64 {
    return 123.0
}

func foo1 (x float64) float64 {
    return x + 123.0
}

func foo2 (x, y float64) float64 {
    return x + y
}

func foo3 (n int, x, y float64) float64 {
    return n*x + y
}

func (x float64) bar1 () float64 {
    return x + 123.0
}

func (x float64) bar2 (y float64) float64 {
    return x + y
}

func (n int) bar3 (x, y float64) float64 {
    return n*x + y
}

func main () {
    a0 := foo0 ()
    print (a0)
    a1 := foo1 (111.0)
    print (a1)
    a2 := foo2 (111.0, 222.0)
    print (a2)
    a3 := foo3 (2, 222.0, 333.0)
    print (a3)
    b1 := (444.0).bar1 ()
    print (b1)
    b2 := (555.0).bar2 (666.0)
    print (b2)
    b3 := (6).bar3 (777.0, 888.0)
    print (b3)
}
