program ProjectConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, math;

function PermMissingElement(A: array of longint; N: longint): longint;
var
    i,suma,x:longint;
begin
    x:=0;
    suma:=0;
    for i:=1 to N+1 do
        x:=x+i;
    for i:=0 to N-1 do
     begin
         suma:=suma+A[i];
     end;
    PermMissingElement:=x-suma;
end;

function TapeEquilibrium(A: array of longint; N: longint): longint;
var
    i,last_min,current_min,leftsum,rightsum,totalsum:longint;
begin
    leftsum:=0;
    rightsum:=0;
    totalsum:=0;
    if(N=1) then TapeEquilibrium:=abs(A[0])
    else
    if(N=2) then TapeEquilibrium:=abs(A[0]-A[1])
    else
    begin
        for i:=0 to N-1 do totalSum := totalSum + A[i];
        last_min := abs(totalSum-A[0]-A[0]);
        for i := 1 to N-1 do
        begin
            leftSum := leftSum + A[i - 1];
            rightSum := totalSum - leftSum;
            current_min := abs(leftSum - rightSum);
            if (current_min < last_min) then last_min := current_min;
        end;
        TapeEquilibrium:= last_min;
    end;
end;

function FindMinimalDifference(tapeArray : Array of longint) : LongInt;
var
  lTotalSum : LongInt;
  lLeftSum  : LongInt;
  lMiniDiff : LongInt;
  lDiff     : LongInt;
  i : Integer;
begin
  lTotalSum := 0;
  lLeftSum  := 0;
  for i := 0 to Length(tapeArray)-1 do
    lTotalSum := lTotalSum + tapeArray[i];
  lMiniDiff := abs(lTotalSum);

  for i := 0 to Length(tapeArray)-1 do
  begin
    lLeftSum := lLeftSum + tapeArray[i];
    lDiff :=  abs(lTotalSum - (2*lLeftSum));
    lMiniDiff := min(lMiniDiff, lDiff);
    if (lMiniDiff=0) then
      break;
  end;
  FindMinimalDifference:= lMiniDiff;
end;

//Trangle
procedure SortIntArray(var IntArray: array of integer);

  function _Compare(Item1, Item2: integer): Integer;
  begin
    Result := Item1 -Item2;
  end;

  procedure _QuickSort(L, R: Integer);
  var
    I, J: Integer;
    P: integer;
    T: integer;
  begin
    repeat
      I := L;
      J := R;
      P := IntArray[(L + R) shr 1];
      repeat
        while _Compare(IntArray[I], P) < 0 do
          Inc(I);
        while _Compare(IntArray[J], P) > 0 do
          Dec(J);
        if I <= J then
        begin
          T := IntArray[I];
          IntArray[I] := IntArray[J];
          IntArray[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
      _QuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  _QuickSort(Low(IntArray), High(IntArray));
end;

function CheckTriangularTripletExists(elementsArray : Array of longint) : LongInt;
var
  p : LongInt;
  q : LongInt;
  r : LongInt;
  i : Integer;
begin
  CheckTriangularTripletExists := 0;
  SortIntArray(elementsArray);
  for i := 0 to Length(elementsArray) do
  begin
    p:= i;
    q:= i+1;
    r:= r+2;
    if r < Length(elementsArray) then
    begin
      if elementsArray[p]+elementsArray[q]>elementsArray[r] then
        CheckTriangularTripletExists := 1
    end
    else
      break;
  end;
end;


Var
  F : File of Longint;
  L : Longint;
  A : Array[0..4] of LongInt;
  N : LongInt;

begin
  A[0]:=2;
  A[1]:=3;
  A[2]:=1;
  A[3]:=5;
  N := 4;
  try
    Write ('This is on the first line ! '); { No CR/LF pair! }
    Writeln ('And this too...');
    Writeln ('But this is already on the second line...');
    Assign (f,'test.tmp');
    Rewrite (f);
    For L:=1 to 10 do
      write (F,L); { No writeln allowed here ! }
    Close (f);

    Writeln(PermMissingElement(A, N));

    A[4]:=4;

    Writeln(TapeEquilibrium(A,5));

    Writeln(FindMinimalDifference(A));

    Writeln(CheckTriangularTripletExists(A));

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  writeln('Done');
  readln;

end.

