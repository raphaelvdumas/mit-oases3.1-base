#!/usr/local/bin/perl

$Pattern=">>>";
open(Fin,$ARGV[0])     ||  die "Input file not found\n";
open(Fout,">$ARGV[1]") ||  die "Cannot create output file\n";
@Insrt=&get_file("insrt.txt");

while (<Fin>) {
    if (/$Pattern/) {		
      print Fout @Insrt;
  } else {
      print Fout $_;
  }			       
}

sub get_file {
    $name=$_[0];
    open(Ft,$name);
    @Ret=<Ft>;
}

