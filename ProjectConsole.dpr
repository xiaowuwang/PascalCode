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

//Alpha 2010

function firstCoveringPrefix(elementsArray : Array of longint) : LongInt;
var
  a1stCoverPrefix, i : Integer;
  occurences : Array of boolean;
begin
  a1stCoverPrefix := 0;
  SetLength(occurences,length(elementsArray));

  if length(elementsArray)>0 then
  begin
    for I := 0 to length(elementsArray)-1 do
    begin
      if not occurences[elementsArray[i]] then
      begin
        occurences[elementsArray[i]] := true;
        a1stCoverPrefix := i;
      end;
    end;
  end;
  firstCoveringPrefix := a1stCoverPrefix;
end;

//Beta 2010

function FUNCintersectingDiscs(elementsArray : Array of longint) : LongInt;
var
  intersectingDiscs, i, N,farthestPossibleDiscsEnd,discEnd,containingDiscs : Integer;
  discsStarts,discsEnds : Array of Integer;
begin
  intersectingDiscs := 0;

  if length(elementsArray)>0 then
  begin
    N:= length(elementsArray);
    SetLength(discsStarts,N);
    SetLength(discsEnds,N);


    //Calculating how many discs starts and ends in every point
    farthestPossibleDiscsEnd := N - 1;
    for I := 0 to N-1 do
    begin
      if i>= elementsArray[i] then
        discsStarts[i - elementsArray[i]]:=discsStarts[i - elementsArray[i]]+1
      else
        discsStarts[0]:=discsStarts[0]+1;
      //If the disc is ending after the N - 1 point we will treat it as ending in N - 1 point.
      discEnd := i + elementsArray[i];
      //The discEnd < 0 check is for arithmetic overflow prevention
      if (discEnd<0)or (discEnd>=N) then
        discsEnds[farthestPossibleDiscsEnd]:=discsEnds[farthestPossibleDiscsEnd]+1
      else
        discsEnds[discEnd]:=discsEnds[discEnd]+1
    end;

    //This will keep track of discs that have started before the current point and will end in this point or after it
    containingDiscs := 0;
    for I := 0 to N-1 do
    begin
        //We increase the number of intersecting discs by:
        //- the number of discs that have been started but not ended before current point multiplied by the number of discs starting at current point (all of them intersect with each other)
        intersectingDiscs := intersectingDiscs +containingDiscs * discsStarts[i];
        //- the number of discs starting at current point multiplied by number of discs starting at current point minus one and this divided by 2 (every disc starting at current point is intersecting with all the others discs starting at current point and we need to avoid counting double intersections)
        intersectingDiscs := intersectingDiscs +(discsStarts[i] * (discsStarts[i] - 1))div 2;

        //If the number of intersecting discs is above 10,000,000 we should return -1
        if (intersectingDiscs > 10000000) then
            intersectingDiscs:= -1;

        //We adjust the number of discs started before the current by adding the number of discs starting in current point and substracting the number of discs ending in current point
        //This way we keep the number of discs starting before or in current point which haven't end yet
        containingDiscs := containingDiscs + discsStarts[i] - discsEnds[i];
    end


  end;
  FUNCintersectingDiscs := intersectingDiscs;
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

