var x = 20;
var first = 0;
var second = 1;

while(x > 0) {
  print first;
  print "\n";
  var next = first + second;
  first = second;
  second = next;
  x = x - 1;
}
