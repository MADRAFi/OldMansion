
procedure Print(s: String);overload;
begin
    Write(s);
end;

procedure Print(s1, s2: String);overload;
begin
    write(s1, s2);
end;

procedure Print(c: char);overload;
begin
    write(X16_VERBATIM_MODE);
    Write(c);
end;

procedure Print(c1, c2: char);overload;
begin
    write(X16_VERBATIM_MODE);write(c1);
    write(X16_VERBATIM_MODE);write(c2);
end;

procedure Print(c: char; s: string);overload;
begin
    write(X16_VERBATIM_MODE);write(c);
    write(s);
end;

procedure Position(x, y: byte);
begin
    gotoxy(x + 1, y + 1);
end;

function Locate(x, y: byte):byte;
begin
    // result:=Scr2Petscii(VPeek(1,VERA_text + (y * $100) + (x * 2)));
    result:=VPeek(1,VERA_text + (y * $100) + (x * 2));
end;

function GetKey:byte;overload;
begin
    result := byte(ReadKey) and %01011111;
end;

function GetKey(a, b:byte):byte;overload;
begin
    result:=0;
    repeat 
        if keypressed then result := byte(ReadKey) and %01011111;
        pause;
    until (result = a) or (result = b);
end;

function GetKey(a, b, c, d: byte):byte;overload;
begin
    result:=0;
    repeat 
        if keypressed then result := byte(ReadKey) and %01011111;
        pause;
    until (result = a) or (result = b) or (result = c) or (result = d);
end;

procedure ClearStatusLines;
begin
    Position(1, 27);
    Write('                                      ');
    Position(1, 28);
    Write('                                      ');
end;

procedure StatusLine(s: String40); overload;
begin
    ClearStatusLines;
    Position(2, 27);
    Write(s);
end;

procedure StatusLine(s1, s2: String40); overload;
begin
    ClearStatusLines;
    Position(2, 27);
    Write(s1, s2);
end;

procedure StatusLine(s1, s2, s3: String40); overload;
begin
    ClearStatusLines;
    Position(2, 27);
    Write(s1, s2, s3);
end;

procedure StatusLine(s1, s2, s3, s4: String40); overload;
begin
    ClearStatusLines;
    Position(2, 27);
    Write(s1, s2, s3, s4);
end;

procedure StatusLine(s1, s2, s3, s4, s5: String40); overload;
begin
    ClearStatusLines;
    Position(2, 27);
    Write(s1, s2, s3, s4, s5);
end;


procedure StatusLine2(s: String40); overload;
begin
    Position(2, 28);
    Write(s);
end;

procedure StatusLine2(s1, s2: String40); overload;
begin
    Position(2, 28);
    Write(s1, s2);
end;

procedure StatusLine2(s1, s2, s3: String40); overload;
begin
    Position(2, 28);
    Write(s1, s2, s3);
end;

procedure StatusLine2(s1, s2, s3, s4: String40); overload;
begin
    Position(2, 28);
    Write(s1, s2, s3, s4);
end;

procedure StatusLine2(s1, s2, s3, s4, s5: String40); overload;
begin
    Position(2, 28);
    Write(s1, s2, s3, s4, s5);
end;