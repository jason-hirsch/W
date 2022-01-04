var x = 1215121;
var curr = 2;
var prime = true;

print x;
print " is ";

while(curr < x && prime) {
  if(x == (x / curr) * curr) {
     print "not prime :(\n";
     prime = false;
  }
  curr = curr + 1;
}

if(prime) {
  print "prime :)\n";
}
