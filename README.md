The represented [aivika-experiment-chart](http://hackage.haskell.org/package/aivika-experiment-chart) package 
complements the [aivika](http://hackage.haskell.org/package/aivika) and 
[aivika-experiment](http://hackage.haskell.org/package/aivika-experiment) packages 
with charting capabilites. Now the simulation results can be represented as charts.

Nevertheless, to plot the charts, you hould use one of the rendering backends
provided by packages [aivika-experiment-cairo](http://hackage.haskell.org/package/aivika-experiment-cairo) 
or [aivika-experiment-diagrams](http://hackage.haskell.org/package/aivika-experiment-diagrams).
While the Cairo-based backend suits mostly to Linux and partially OS X, the Diagrams-based 
backend is mainly destined for MS Windows, although it should work on Linux 
and OS X too.
