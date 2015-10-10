(* IndexGen:                                     HDO, 2002-02-28 *)
(* --------                                                      *)
(* Generation of a sorted index of all words in a text file.     *)
(* Command line:                                                 *)
(*    IndexGen [ textFileName ]                                  *)
(*===============================================================*)
PROGRAM IndexGen;

  USES
    Crt, Timer;

  CONST
    EF = CHR(0);         (*end of file character*)
    maxWordLen = 30;     (*max. number of characters per word*)
    chars = ['a' .. 'z', 'ä', 'ö', 'ü', 'ß',
             'A' .. 'Z', 'Ä', 'Ö', 'Ü'];

  TYPE
    Word = STRING[maxWordLen];

    Node = ^NodeRec;
    NodeRec = RECORD
      key : STRING;
      lnr : INTEGER;
      next: Node;
    END;

    HashTable = ARRAY OF Node;

  VAR
    txt, UserFile: TEXT;           (*text file*)
    curLine: STRING;     (*current line from file txt*)
    curCh: CHAR;         (*current character*)
    curLineNr: INTEGER;  (*current line number*)
    curColNr: INTEGER;   (*current column number*)
    table : HashTable;

  PROCEDURE DisposeList(VAR l : Node);
  VAR
    cur : Node;
  BEGIN
    IF l <> NIL THEN BEGIN
      cur := NIL;
      WHILE l <> NIL DO BEGIN
        cur := l^.next;
        Dispose(l);
        l := NIL;
        l := cur;
      END;
    END;
    l := NIL;
  END;

  PROCEDURE DisposeHashTable;
  VAR
    i : INTEGER;
  BEGIN
    FOR i := 0 TO High(table) DO BEGIN
      DisposeList(table[i]);
      Dispose(table[i]);
      table[i] := NIL;
    END;
  END;

  FUNCTION LowerCase(ch: CHAR): STRING;
  BEGIN
    CASE ch OF
      'A'..'Z': LowerCase := CHR(ORD(ch) + (ORD('a') - ORD('A')));
      'Ä', 'ä': LowerCase := 'ae';
      'Ö', 'ö': LowerCase := 'oe';
      'Ü', 'ü': LowerCase := 'ue';
      'ß':      LowerCase := 'ss';
      ELSE (*all the others*)
                LowerCase := ch;
      END; (*CASE*)
  END; (*LowerCase*)

FUNCTION NewNode(key:STRING; lnr:INTEGER):Node;
VAR
  n : NODE;
BEGIN
  New(n);
  n^.key := key;
  n^.lnr := lnr;
  n^.next := NIL;
  NewNode := n;
END;

PROCEDURE InitHashTable;
VAR
  i : INTEGER;
BEGIN
  FOR i := 0 TO High(table) DO BEGIN
    table[i] := NIL;
  END;
END;

 PROCEDURE GetNextChar; (*updates curChar, ...*)
  BEGIN
    IF curColNr < Length(curLine) THEN BEGIN
        curColNr := curColNr + 1;
        curCh := curLine[curColNr]
      END (*THEN*)
    ELSE BEGIN (*curColNr >= Length(curLine)*)
      IF NOT Eof(txt) THEN BEGIN
          ReadLn(txt, curLine);
          curLineNr:= curLineNr + 1;
          curColNr := 0;
          curCh := ' '; (*separate lines by ' '*)
        END (*THEN*)
      ELSE (*Eof(txt)*)
        curCh := EF;
    END; (*ELSE*)
  END; (*GetNextChar*)

  PROCEDURE GetNextWord(VAR w: Word; VAR lnr: INTEGER);
  BEGIN
    WHILE (curCh <> EF) AND NOT (curCh IN chars) DO BEGIN
      GetNextChar;
    END; (*WHILE*)
    lnr := curLineNr;
    IF curCh <> EF THEN BEGIN
        w := LowerCase(curCh);
        GetNextChar;
        WHILE (curCh <> EF) AND (curCh IN chars) DO BEGIN
          w := Concat(w , LowerCase(curCh));
          GetNextChar;
        END; (*WHILE*)
      END (*THEN*)
    ELSE (*curCh = EF*)
      w := '';
  END; (*GetNextWord*)


PROCEDURE Swap(a,b : INTEGER);
VAR
  c : Node;
BEGIN
  c := table[a];
  table[a] := table[b];
  table[b] := c;
END;

 (* Less than *)
FUNCTION LT (a,b : STRING) : BOOLEAN;
BEGIN
  LT := a < b;
END;

PROCEDURE QuickSort(l, u : INTEGER);
VAR
  i, j : INTEGER;
  p : STRING;
  
 BEGIN
  (* nothing to do for lists of size 1 or 0 *)
    IF (u-l) > 0 THEN BEGIN
      WHILE table[l] = NIL DO BEGIN
        l := l + 1;
      END;
      p := table[l]^.key; (* select first element *)
      i := l;
      j := u;
      WHILE i <= j DO BEGIN
          WHILE (table[i] <> NIL) AND (i <= u) AND LT(table[i]^.key, p) DO Inc(i);
          WHILE (table[j] <> NIL) AND (j >= l) AND LT(p, table[j]^.key) DO Dec(j);
          IF i < j THEN Swap(i, j);
          Inc(i);
          Dec(j);

      END;
      ASSERT(i>j);
      IF l < j THEN BEGIN
        QuickSort(l, j);
      END;
      IF i < u THEN BEGIN
        QuickSort(i,u);
      END;
    END;
END;


(* Write hashtable to file 
   txtfile: Filename for generated file
 *)
PROCEDURE WriteToFile(txtfile : STRING);
VAR 
  Txt : STRING;
  tableNode : NODE;
  i : INTEGER;
BEGIN

  (* ONLY FOR MAC USER (use absolut path) *)
  Assign(UserFile, 'Hagenberg/ADF & PRG/Uebung1/'+txtfile+'.txt'); {assign a text file}

  (* ONLY FOR WINDOWS USER *)
  (*Assign(UserFile, txtfile+'.txt'); {assign a text file} *)
  
  Rewrite(UserFile); {open the file 'fname' for writing}
   (*Writeln(UserFile,'PASCAL PROGRAMMING');*)

  FOR i := 0 TO High(table) DO BEGIN
    IF table[i] <> NIL THEN BEGIN
      tableNode := table[i];
      
      Write(Userfile, 'Wort: ', tableNode^.key);
      WHILE tableNode <> NIL DO BEGIN

        Write(UserFile, ', ', tableNode^.lnr);
        tableNode := tableNode^.next;
      END; 
      WriteLn(Userfile, '');
    END;
  END;

 Close(UserFile);
END;

(* Generates Hash-Code from String *)
FUNCTION GetHashCode(key:STRING):INTEGER;
VAR
  hc, i : INTEGER;
BEGIN
  hc := 0;
  (*$R-*)
  FOR i := 1 TO Length(key) DO BEGIN
    hc := 31 * hc + Ord(key[i]);
  END;
  (*$R+*)
  GetHashCode := Abs(hc) MOD Length(table);
END;

  VAR
    txtName: STRING;
    w: Word;        (*current word*)
    lnr: INTEGER;   (*line number of current word*)
    n: LONGINT;     (*number of words*)

    length_table : INTEGER;
    hash : INTEGER;
    e, new : Node;

BEGIN (*IndexGen*)
  (* Set dynamic Array Length *)
  SetLength(table, 32000);
  InitHashTable;

  Write('IndexGen: index generation for text file ');

  IF ParamCount = 0 THEN BEGIN
    WriteLn;
    WriteLn;
    Write('name of text file > ');
    ReadLn(txtName);
  END (*THEN*)
  ELSE BEGIN
    txtName := ParamStr(1);
    WriteLn(txtName);
  END; (*ELSE*)
  WriteLn;

  (*--- read text from text file ---*)

  (* ONLY FOR MAC USER (use absolut path) *)
  Assign(txt, 'Hagenberg/ADF & PRG/Uebung1/' + txtName);
  
  (* ONLY FOR WINDOWS USER  *)
  (*Assign(txt, txtName);*)

  Reset(txt);
  curLine := '';
  curLineNr := 0;
  curColNr := 1; (*curColNr > Length(curLine) forces reading of first line*)
  GetNextChar;   (*curCh now holds first character*)

  StartTimer;
  GetNextWord(w, lnr);
  n := 0;

  length_table := High(table);
  hash := 0;
  InitHashTable;

  WHILE Length(w) > 0 DO BEGIN
    WriteLn(w, ' ', lnr);

    hash := GetHashCode(w);

    e := table[hash];

    new := NewNode(w, lnr);

    (* Set List for Array-Entry *)
    IF e <> NIL THEN BEGIN
      WHILE e^.next <> NIL DO BEGIN
        e := e^.next;
      END;
      e^.next := new;
    END ELSE BEGIN
      e := new; 
    END;

    table[hash] := e;

    n := n + 1;
    GetNextWord(w, lnr);
  END; (*WHILE*)

  WriteToFile('vor');
  
  QuickSort(1, High(table));

  WriteToFile('nach');

  StopTimer;
  
  DisposeHashTable;

  WriteToFile('dispose');

  WriteLn;
  WriteLn('number of words: ', n);
  WriteLn('elapsed time:    ', ElapsedTime);
  Close(txt);
  
  ReadLn;

END. (*IndexGen*)