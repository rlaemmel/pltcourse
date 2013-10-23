% 
% Agent equations for sample system
%  The producer keeps on say foo and bar.
%  The consumer consumes such foos and bars and replies with blas.
%

equation(
  system,
  (name(producer)|name(consumer))).

equation(
  producer,
  ((out(foo),name(producer))+(out(bar),name(producer)))).

equation(
  consumer,
  ((in(foo),(out(bla),name(consumer)))+(in(bar),(out(bla),name(consumer))))).
