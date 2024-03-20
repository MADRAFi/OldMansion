program oldmansion;
uses x16, x16_zsmkit, x16_vera, crt;

type String40 = string[40];

const
{$i const.inc}

var
    inventory, currentPassword, weaponName: String40;
    aStr, bStr: String40; // reusable temporary strings
    x, y: byte; // player Position
    i: byte; // common iterator
    monster: byte;
    consecutiveWaits, seenPassword: byte;
    currentRoomTile, keycode, roomDifficulty, maxDifficulty: byte;
    q, r: byte;  // randoms
    weapon, wounds: shortInt;
    moves, gold: smallInt;
    score: cardinal;
    pconsol: byte;
    strength, energy, monsterStrength, monsterSize: real;
    gameEnded: boolean;
    lootHistory: array[0..ITEMS_COUNT-2] of byte;
    music: boolean;

{$i io_x16.inc}
{$i lang_en.inc}


procedure StartMusic(bank: byte; addr: word; priority: byte);
begin
    zsmSetMem(priority, bank, addr);
    zsmPlay(priority);
end;

procedure StopMusic(priority: byte);
begin
    zsmStop(priority);
end;

procedure StartEffect(bank: byte; addr: word; priority: byte);
begin
    if not music then begin
        zsmSetMem(priority, bank, addr);
        zsmPlay(priority);
    end;
end;

procedure fadeIn;
begin
    veraDirectLoadPalette('assets/default.pal');
end;

// procedure fadeOut;
// begin
//     veraFade(0);
// end;


procedure fadeOut; assembler;
asm
        jmp start

        PTR = $22

        FadeSpeed               = 200    ;higher is slower, max 255
        RambankForPaletteData   = 30     ;Rambank which holds the original palette data for fading in


        tmp:  .byte 0
        tmp2: .byte 0
        FadeDirection: .byte 0  ;if=0>fade out, if=1->fade in


        ;not nececary, but makes reading easier
        NibbleLeftNew:    .byte 0
        NibbleRightNew:   .byte 0
        NibbleLeftOrig:   .byte 0
        NibbleRightOrig:  .byte 0

        start:
        // jsr CopyPaletteToHighmem


        FadeIndef:
            lda #0
            sta FadeDirection
            jsr Fade
            // lda #1
            // sta FadeDirection  
            // jsr Fade
        
        // jmp FadeIndef


        rts

        Fade:
        ldx #0
        NextFadeStep:
            jsr Delay
            jsr FadeOneStep

            inx
            cpx #15     ;4 bits, so 16 steps should result in all black
            bne NextFadeStep
        rts



        FadeOneStep:
        ;prepare pointer to check max palette color values 
        lda #$00
        sta PTR
        lda #$a0
        sta PTR+1
        
        lda $00  ;save current rambank and restore when done
        pha
            ;set rambank to RambankForPaletteData
            lda #RambankForPaletteData
            sta $00
            
            phx
                ;set vera address to palette offset, no auto increment
                lda #$00
                sta VERA_addr_low
                lda #$FA
                sta VERA_addr_high
                lda #$01
                sta VERA_addr_bank
                
                ldy #0

                NextY:
                ldx #0

                NextX:
                    phy
                    lda VERA_data0          ;get byte from p
                    
                    ldy FadeDirection
                    cpy #0
                    bne FadeIn
                        ;FadeOut
                        pha
                            ;decrement the right nibble
                            sec
                            sbc #$01
                            AND #$0F ;discard the left nibble
                            sta tmp
                        pla   
                        
                        ;decrement the left nible
                        sec
                        sbc #$10
                        AND #$F0    ;discard the right nible
                        ora tmp     ;merge the new left and right nibble
                        
                        jsr CheckNegative ;check if any of the nibbles has become negarive, if so set nibble to 0
                        jmp StoreNewColorData
                    FadeIn:
                    
                        ;FadeIn
                        pha
                            ;increment the right nibble
                            clc
                            adc #$01
                            AND #$0F ;discard the left nibble
                            sta tmp
                        pla   
                        
                        ;increment the left nible
                        clc
                        adc #$10
                        AND #$F0    ;discard the right nible
                        ora tmp     ;merge the new left and right nibble
                        jsr CheckWithOriginalPalette
                    
                    StoreNewColorData:
                    
                    sta VERA_data0 ;store new palette color values
                    
                    ;Increment VERA address
                    inc VERA_addr_low       ;increase low byte
                    bne DoNotIncHighByte    
                    inc VERA_addr_high   ;inc high byte if low byte became zero, we do not care about the 3rd byte
                    DoNotIncHighByte:
                    
                    ;Increment PTR to palette in rambank
                    inc PTR
                    bne DoNotIncHighBytePtr
                    inc PTR+1
                    DoNotIncHighBytePtr:
                    
                    ply
                    
                    
                    inx
                    cpx #0      ;256 times
                    bne NextX
                    iny
                    cpy #2      ;times 2 = the full pallete
                    bne NextY
            plx      
        pla
        sta $00
        rts
        
        CheckNegative:
        ;check if a nibble has become negative,  then force zero
        sta tmp2
        
        AND #$F0
        cmp #$F0
        bne NotNegativeLeft
        lda tmp2
        AND #$0F ;negative, so set left nibble to zero
        sta tmp2
        NotNegativeLeft:
        
        ;check right nible
        lda tmp2
        AND #$0F
        cmp #$0F
        bne NotNegativeRight      
        lda tmp2
        AND #$F0
        sta tmp2
        NotNegativeRight:
        lda tmp2
        rts



        CheckWithOriginalPalette: 
        ;check the new palette color values against the original palette in highram, 
        ;and cap the max value per nibblw
        
        sta tmp
        AND #$F0
        sta NibbleLeftNew
        lda tmp
        AND #$0F
        sta NibbleRightNew  

        lda (PTR)   
        sta tmp
        AND #$F0
        sta NibbleLeftOrig
        lda tmp
        AND #$0F
        sta NibbleRightOrig
        
        lda NibbleLeftNew
        cmp NibbleLeftOrig
        bcc DoNotCapLeft
            lda NibbleLeftOrig
            sta NibbleLeftNew

        DoNotCapLeft:
        lda NibbleRightNew
        cmp NibbleRightOrig
        bcc DoNotCapRight
            lda NibbleRightOrig
            sta NibbleRightNew

        DoNotCapRight:
        lda NibbleLeftNew
        ora NibbleRightNew
        
        
        rts




        CopyPaletteToHighmem:
        
        lda $00
        pha
            lda #RambankForPaletteData   
            sta $00  ;set rambank #1
            
            ;set vera address to pallette offset, auto increment by 1
            lda #$00
            sta VERA_addr_low
            lda #$FA
            sta VERA_addr_high
            lda #$11
            sta VERA_addr_bank  
            
            ;set from address
            lda #<VERA_data0
            sta r0
            lda #>VERA_data0
            sta r0+1
            
            ;set destination
            lda #$00
            sta r1
            lda #$a0
            sta r1+1
            
            ;set number of bytes $200 = 521
            lda #$00
            sta r2
            lda #$02
            sta r2+1
            
            jsr $FEE7   ;kernal function memory_copy
        pla
        sta $00
        

        rts

        Delay:
        phy
        phx
        ldx #0
        @NextX:
            ldy #0
            @NextY:
                iny
                cpy #0
                bne @NextY
                inx
                cpx #FadeSpeed
                bne @NextX
        plx
        ply
        rts
end;


// ************************************* helpers


function StrCmp(a, b: TString): boolean;
var i:byte;
begin
    result:= true;
    for i:=0 to length(a)-1 do
        if a[i]<>b[i] then exit(false);
end;

function FormatFloat(num: real):TString;
var m: cardinal;
    ms: TString;
begin
    Str(Trunc(num), result);
    m := Trunc(Frac(num) * 1000.0);
    if m > 0 then begin
        Str(m, ms);
        while (length(ms) < 3) do ms := concat('0', ms);
        result := concat(result, '.');
        result := concat(result, ms);
        while (result[byte(result[0])]='0') do dec(result[0]);
    end;
end;


// ************************************* initializers

procedure VarInit;
begin
    x := 6;
    y := 1;
    roomDifficulty := 1;
    maxDifficulty := 15 * 8;
    fillchar(@inventory[1], INVENTORY_SIZE, TILE_EMPTY_SLOT);
    inventory[0] := char(INVENTORY_SIZE);
    currentPassword := passwords[Random(PASSWORDS_COUNT)];
    consecutiveWaits := 0;
    weapon := 1;
    gold := 0;
    wounds := 0;
    energy := 3;
    seenPassword := 0;
    moves := 0;
    currentRoomTile := TILE_ROOM;
    weaponName := weapons[weapon - 1];
    fillByte(@lootHistory, ITEMS_COUNT - 1, 0);

    // x := 34;
    // y := 13;
    // weapon := 8;
    // gold := 1000;
    // energy := 3000;
    // currentPassword := passwords[0];
end;

// ************************************* GUI

procedure PromptAny;
begin
   Position((79 - Length(s_PRESS_ANY)) div 2, 57);
   Write(s_PRESS_ANY);
   repeat until keypressed;
   ClrScr;
   Position(0,0);
end;

procedure TitleScreen;
var
    i:byte;
    colors: array[0..3] of byte = (
        X16_COLOR_WHITE, X16_COLOR_LIGHT_GREY, X16_COLOR_GREY, X16_COLOR_DARK_GREY
    );
begin
    veraGraphInit;
    veraDirectLoadImage('assets/mplogo.bin');
    Pause(3);
    fadeIn;
    Pause(200);
    fadeOut;
    veraDirectLoadImage('assets/oldmansion.bin');
    fadeIn;
    i:=0;
    repeat 
        TextColor(colors[i]);
        Position(13,25);
        Print('Press any key');
        Inc(i);
        if (i = 3) then i:=0;
        pause(8);
        if keypressed then begin
            keycode:=byte(ReadKey);
            IF keycode = k_F10 then begin
                music:=not music;
                StopMusic(0);
            end;
        end;
    until keypressed;
    fadeOut;
end;

procedure ShowManual;
begin
    TextMode(X16_MODE_80x60);
    write(X16_ISO_ON);
    TextCharset('assets/om_iso_manual.fnt');
    TextBackground(X16_COLOR_BLACK);
    TextColor(X16_COLOR_WHITE);
    ClrScr;
    Position((79 - Length(s_WANT_MANUAL)) div 2, 3);
    Writeln(s_WANT_MANUAL);
    fadeIn;
    keycode := GetKey(k_YES, k_NO);
    fadeOut;
    if keycode <> k_YES then exit;
    // Status frame
    TextBackground(X16_COLOR_BLACK);
    TextColor(X16_COLOR_GREY);
    Position(0, 0); // top row
    Print(char(TILE_FRAME1_TOPLEFT));
    for i:=1 to 78 do
        Print(char(TILE_FRAME1_H));
    Print(char(TILE_FRAME1_TOPRIGHT));

    Position(0, 1);
    for i:=0 to 57 do
    begin
        Position(0, 1+i);
        Print(char(TILE_FRAME1_V));
        Position(79, 1+i);
        Print(char(TILE_FRAME1_V));
    end;


    Position(0, 59); // bottom row
    Print(char(TILE_FRAME1_BOTTOMLEFT));
    for i:=1 to 78 do
        Print(char(TILE_FRAME1_H));
    VPoke(1,VERA_text + ($100*59)+(79*2),TILE_FRAME1_BOTTOMRIGHT);
    VPoke(1,VERA_text + ($100*59)+(79*2)+1, $0c);

    ManualPage_C80;
    fadeIn;
    PromptAny;
    fadeOut;
end;

function GetRandomEntranceV:byte;
var r: byte;
begin
    result := TILE_ENTRANCE_V;
    r := Random(5);
    if r = 0 then result := TILE_DOOR_V;
    if r = 1 then result := TILE_WALL_V;
end;

function GetRandomEntranceH:byte;
var r: byte;
begin
    result := TILE_ENTRANCE_H;
    r := Random(5);
    if r = 0 then result := TILE_DOOR_H;
    if r = 1 then result := TILE_WALL_H;
end;



procedure ShowStats;
var z: real;
begin
    TextBackground(X16_COLOR_BLACK);
    TextColor(X16_COLOR_WHITE);

    if energy < 0 then energy := 0;
    Position(2, 19); Write(s_ENERGY,' '); Write(formatFloat(energy), '    ');
    Position(22, 19); Write(s_WOUNDS, ' ');Write(wounds, '    ');
    z := energy - wounds;
    if z < 0 then z := 0.1;
    Position(2, 21); Write(s_TREASURE,' $'); Write(gold, '    ');
    Position(22, 21); Write(s_ITEMS,' ');
    
    Write(inventory);
    
    Position(2, 23); Write(s_WEAPON, ' '); Write(weaponName,'     ');
    strength := z * (1 + weapon * 0.25);
    Position(2, 25); Write(s_ATTACK, ' '); Write(formatFloat(strength), '          ');
    Position(22, 25); Write(s_MOVES, ' '); Write(moves,' ');

end;

procedure PaintBoard;
var row, room:byte;
begin

    // fadeOut;
    TextMode(X16_MODE_40x30);
    write(X16_ISO_ON);
    TextBackground(X16_COLOR_BLACK);
    TextColor(X16_COLOR_WHITE);
    ClrScr;

    TextCharset('assets/om_iso_field.fnt');
    TextBackground(X16_COLOR_ORANGE);
    TextColor(X16_COLOR_DARK_GREY);
    

    VarInit;

    Position(5, 0); // top row of board
    Print(char(TILE_BORDER_TOPLEFT));
    for room:=1 to 14 do
        Print(char(TILE_BORDER_H), char(TILE_BORDER_MIDDLETOP));
    Print(char(TILE_BORDER_H), char(TILE_BORDER_TOPRIGHT));

    for row:=0 to 7 do begin

        // rooms
        Position(5, row * 2 + 1);
        Print(char(TILE_BORDER_V));
        for room:=1 to 14 do
            Print(char(TILE_ROOM), char(GetRandomEntranceV));
        Print(char(TILE_ROOM), char(TILE_BORDER_V));

        // inner walls
        Position(5, row * 2 + 2);
        Print(char(TILE_BORDER_MIDDLELEFT));
        for room:=1 to 14 do
            Print(char(GetRandomEntranceH), char(TILE_BORDER_MIDDLE));
        Print(char(GetRandomEntranceH), char(TILE_BORDER_MIDDLERIGHT));

    end;


    Position(5, 16); // bottom row of board
    Print(char(TILE_BORDER_BOTTOMLEFT));
    for room:=1 to 14 do Print(char(TILE_BORDER_H), char(TILE_BORDER_MIDDLEBOTTOM));
    Print(char(TILE_BORDER_H), char(TILE_BORDER_BOTTOMRIGHT));

    Position(7, 1); Print(char(TILE_ENTRANCE_V));
    Position(6, 2); Print(char(TILE_ENTRANCE_H));
    Position(x, y); Print(char(TILE_PLAYER));
    Position(34, 15); Print(char(TILE_EXIT),char(TILE_EXIT2));


    // Status frame
    TextBackground(X16_COLOR_BLACK);
    TextColor(X16_COLOR_GREY);
    Position(0, 17); // top row
    Print(char(TILE_FRAME1_TOPLEFT));
    for i:=1 to 37 do
        Print(char(TILE_FRAME1_H));
    Print(char(TILE_FRAME1_H), char(TILE_FRAME1_TOPRIGHT));

    Position(0, 18);
    for i:=0 to 10 do
    begin
        Position(0, 18+i);
        Print(char(TILE_FRAME1_V));
        Position(39, 18+i);
        Print(char(TILE_FRAME1_V));
    end;


    Position(0, 29); // bottom row
    Print(char(TILE_FRAME1_BOTTOMLEFT));
    for i:=1 to 38 do
        Print(char(TILE_FRAME1_H));
    // Print(char(TILE_FRAME1_BOTTOMRIGHT));
    VPoke(1,VERA_text + ($100*29)+(39*2),TILE_FRAME1_BOTTOMRIGHT);
    VPoke(1,VERA_text + ($100*29)+(39*2)+1, $0c);

    // Pause;
    ShowStats;
    fadeIn;
end;

procedure KeyAndShowStat;
begin
    // Readkey;
    repeat until keypressed;
    ShowStats;
end;


// ************************************* inventory operations


function HasItem(b: byte):boolean;
var i: byte;
begin
    result := false;
    for i := 1 to INVENTORY_SIZE do
        if inventory[i] = char(b) then exit(true);
end;

procedure DelItem(b: byte);
var i: byte;
begin
    for i := 1 to INVENTORY_SIZE do
        if inventory[i] = char(b) then begin
            inventory[i] := char(TILE_EMPTY_SLOT);
            exit;
        end;
end;

procedure AddItem(b: byte);
var i: byte;
begin
    for i := 1 to INVENTORY_SIZE do
        if inventory[i] = char(TILE_EMPTY_SLOT) then begin
            inventory[i] := char(b);
            exit;
        end;
end;

function HasAnythingToUse:boolean;
begin
    result := HasItem(itemSymbols[4]) or HasItem(itemSymbols[5]) or HasItem(itemSymbols[6]) or HasItem(itemSymbols[7]);
end;

// ********************************************** main turn logic

procedure MakeMove;
var door, room: byte;
    dx, dy: shortInt;
    isIn, stepFinished, waited, skipMonster: boolean;
    itemLost: byte;


procedure PayRansom;
begin
    gold := round(gold - round(monsterSize * Random));  // pay ransom
    if gold < 0 then gold := 0;
    stepFinished := true;
    ShowStats;
end;

procedure FoundWeapon;
begin
    r := Random(10) + 1;    // 1-10
    StatusLine(s_FOUND, weapons[r - 1]);
    StartEffect(61, $A700, 3);
    Position(10, 28); Write(s_TAKE, s_OR, s_LEAVE, ' ?');
    keycode := getKey(k_TAKE, k_LEAVE);
    if keycode = k_TAKE then begin
        weaponName := weapons[r - 1];
        weapon := r;
        ShowStats;
    end;
end;

procedure FoundPassword;
begin
    if seenPassword < 3 then begin
        StartEffect(61, $A400, 3);
        StatusLine(s_FOUND_PASS, currentPassword, '.');
        StatusLine2(s_REMEMBER);
        Pause(200);
        inc(seenPassword);
    end;
end;

procedure DecLootCounters;
var item:byte;
begin
    for item:=0 to ITEMS_COUNT-2 do
        if lootHistory[item]>0 then Dec(lootHistory[item]);
end;

function GetMonster: byte;
var monsterLevel: shortInt;
begin
    monsterLevel := (roomDifficulty * MONSTERS_COUNT) div maxDifficulty;
    monsterLevel := monsterLevel - 4 + Random(8);
    if monsterLevel < 0 then monsterLevel := 0;
    monsterStrength := Round(Random(byte((monsterLevel * 2) + 1)) + (roomDifficulty + strength / 10)) + 1;
    monsterSize := monsterStrength;
    if monsterLevel >= MONSTERS_COUNT then monsterLevel := MONSTERS_COUNT - 1;
    result := monsterLevel;
end;

procedure FoundItem;
var
    item:byte;
begin
    DecLootCounters;
    repeat
        item := Random(ITEMS_COUNT-1);
    until lootHistory[item] = 0;
    lootHistory[item] := 6;
    StatusLine(s_FOUND, items[item]);
    StartEffect(61, $A600, 3);
    Position(10, 28); Write(s_TAKE, s_OR, s_LEAVE, ' ?');
    keycode:= GetKey(k_TAKE, k_LEAVE);
    if keycode = k_LEAVE then begin
        FoundWeapon;
    end else begin
        // if (item = 4) or (item = 5) then begin
            // case item of
            //     4: energy := energy + 3;
            //     5: energy := energy + 1;
            // end;
            // ShowStats;
        // end else begin
            if hasItem(TILE_EMPTY_SLOT) then begin
                addItem(itemSymbols[item]);
                ShowStats;
            end else begin
                stepFinished := false;
                repeat
                    StatusLine2(s_LEAVE_WHAT);
                    keycode:=0;
                    repeat
                        if keypressed then keycode := getKey;
                        pause;
                    until (keycode > 65) and (keycode < 90);
                    case keycode of
                        k_HAMMER:    i:=TILE_HAMMER;
                        k_LANTERN:   i:=TILE_LANTERN;
                        k_KEY:       i:=TILE_KEY;
                        k_PLANK:     i:=TILE_PLANK;
                        k_FOOD:      i:=TILE_FOOD;
                        k_DRINK:     i:=TILE_DRINK;
                        k_BANDAGE:   i:=TILE_BANDAGES;
                        k_MEDICINES: i:=TILE_MEDICINES;
                    end;
                    if hasItem(i) then begin
                        DelItem(i);
                        addItem(itemSymbols[item]);
                        ShowStats;
                        stepFinished:=true;
                    end else begin
                        StatusLine2(s_DONT_HAVE, char(i), s_ANY);
                        KeyAndShowStat;
                    end;
                until stepFinished;
            end;
        // end;
    end;
end;

procedure GetLoot();
begin
    r := Random(11) + 1;
    case r of
        1,2,3,4,5,6,7,8: FoundItem;
        9: FoundWeapon;
        10: FoundPassword;
    end;
end;

procedure MovePlayer(nx, ny:byte);
begin
    TextBackground(X16_COLOR_ORANGE);
    TextColor(X16_COLOR_DARK_GREY);
    consecutiveWaits := 0;
    Position(x, y);
    Print(char(currentRoomTile));
    x := nx;
    y := ny;
    currentRoomTile := Locate(x, y);
    Position(x, y);
    Print(char(TILE_PLAYER));
    roomDifficulty := ((x - 4) shr 1) * ((y + 1) shr 1);
    TextBackground(X16_COLOR_BLACK);
    TextColor(X16_COLOR_WHITE);
end;

begin
    ShowStats;
    isIn := false;
    stepFinished := false;
    skipMonster := false;
    waited := false;
    // keycode:=0;
    ClearStatusLines;
    Position(10, 27); Write(s_WAIT, s_OR, s_MOVE, ' ?');
    keycode := getKey(k_REST, k_MOVE);

    if keycode = k_REST then begin      // ************* waiting
        if gold < 5 then begin
            energy := energy + 0.5;
        end else begin
            gold := gold - 5;
            energy := energy + 2;
        end;
        waited := true;
        stepFinished := true;
        ShowStats;
        Inc(consecutiveWaits);

    end else begin                  // ************* moving
        Inc(consecutiveWaits);
        Position(4, 27);
        Write(s_LEFT,', ',s_RIGHT,', ',s_UP,', ',s_DOWN,' ?');
        keycode := getKey(k_LEFT, k_RIGHT, k_UP, k_DOWN);
        ClearStatusLines;
        dx := 0;
        dy := 0;
        case keycode of
            k_LEFT: dx := -1;
            k_RIGHT: dx := 1;
            k_DOWN: dy := 1;
            k_UP: dy := -1;
        end;

        door := Locate(x + dx, y + dy);
        
        // Str(door,aStr);
        // StatusLine2('door ',aStr);
        // repeat until keypressed;

        if (door <> TILE_BORDER_H) and (door <> TILE_BORDER_V) then begin // not a border ?

            if (door = TILE_ENTRANCE_H) or (door = TILE_ENTRANCE_V) then begin     // ***********************  check doors
                StatusLine(s_DOOR_OPENED, s_ANY);
                isIn := true;
            end else begin
                if (door = TILE_DOOR_H) or (door = TILE_DOOR_V) then begin
                    StatusLine(s_DOOR_CLOSED);
                    if hasItem(itemSymbols[2]) then begin
                        StatusLine2(s_USED, s_KEY, '.', s_ANY);
                        isIn := true;
                    end else begin
                        StatusLine2(s_DONT_HAVE, s_BYKEY, '.', s_ANY);
                    end;
                end;
                if (door = TILE_WALL_H) or (door = TILE_WALL_V) then begin
                    StatusLine(s_WALL);
                    if hasItem(itemSymbols[0]) then begin
                        StatusLine2(s_USED, s_HAMMER, '.', s_ANY);
                        isIn := true;
                    end else begin
                        StatusLine2(s_DONT_HAVE, s_BYHAMMER, '.', s_ANY);
                    end;
                end;
                if isIn then begin
                    energy := energy - 0.5;
                    Position(x + dx, y + dy);
                    if dy=0 then door := TILE_ENTRANCE_V
                    else door := TILE_ENTRANCE_H;
                    TextBackground(X16_COLOR_ORANGE);
                    TextColor(X16_COLOR_DARK_GREY);
                    Print(char(door));
                end;
            end;

            //ReadKey;
            repeat until keypressed;

            if isIn then begin  // *******************************   check room
                room := Locate(x + 2 * dx, y + 2 * dy);
                if room = TILE_ROOM then begin
                    q := Random(2);
                    if not waited and (x > 8) and (y > 3) and (Random(10) >= 4) then begin
                        if q = 0 then room := TILE_DARK
                        else room := TILE_HOLE;
                    end else begin
                        r := Random(6);
                        if r = 0 then begin
                            if q = 0 then room := TILE_DARK
                            else room := TILE_HOLE;
                        end;
                    end;
                end;

                if room = TILE_DARK then begin    //  ********************* dark room
                    Position(x + 2 * dx, y + 2 * dy);
                    TextBackground(X16_COLOR_ORANGE);
                    TextColor(X16_COLOR_DARK_GREY);
                    Print(char(TILE_DARK));
                    TextBackground(X16_COLOR_BLACK);
                    TextColor(X16_COLOR_WHITE);
                    StatusLine(s_ROOM_DARK);
                    if hasItem(itemSymbols[1]) then begin
                        StatusLine2(s_USED, s_LANTERN, '.', s_ANY);
                        isIn := true;
                    end else begin
                        StatusLine2(s_DONT_HAVE, s_BYLANTERN, '.', s_ANY);
                        isIn := false;
                    end;
                    repeat until keypressed;
                    ClearStatusLines;
                end;
                if room = TILE_HOLE then begin    //  ********************* no floor
                    Position(x + 2 * dx, y + 2 * dy);
                    TextBackground(X16_COLOR_ORANGE);
                    TextColor(X16_COLOR_DARK_GREY);
                    Print(char(TILE_HOLE));
                    TextBackground(X16_COLOR_BLACK);
                    TextColor(X16_COLOR_WHITE);
                    StatusLine(s_ROOM_HOLE);
                    if hasItem(itemSymbols[3]) then begin
                        StatusLine2(s_USED, s_PLANK, '.', s_ANY);
                        isIn := true;
                    end else begin
                        StatusLine2(s_DONT_HAVE, s_BYPLANK, '.', s_ANY);
                        isIn := false;
                    end;
                    repeat until keypressed;
                end;
                if room = TILE_EXIT then begin  //  ********************* exit reached
                    Position(x, y);
                    TextBackground(X16_COLOR_ORANGE);
                    TextColor(X16_COLOR_DARK_GREY);
                    Print(char(TILE_ROOM));
                    Position(34, 15);
                    Print(char(TILE_PLAYER));
                    TextBackground(X16_COLOR_BLACK);
                    TextColor(X16_COLOR_WHITE);
                    StatusLine(s_EXIT_PASS);
                    aStr:='';
                    Position(2, 28);
                    for i := 1 to 4 do begin
                        keycode:=0;
                        repeat
                            if keypressed then keycode := getKey;
                            pause;
                        until keycode<>0;
                        aStr[i] := char(keycode);
                        TextBackground(X16_COLOR_BLACK);
                        TextColor(X16_COLOR_WHITE);
                        Print(char(keycode));
                    end;
                    aStr[0] := #4; //password length
                    if strCmp(aStr, currentPassword) then begin
                        StatusLine(s_EXIT_PAY, s_ANY);
                        repeat until keypressed;
                        if gold >= 100 then begin
                            gold := gold - 100;
                            score := Trunc(gold * weapon);
                            Str(gold, aStr);
                            Str(score, bStr);
                            StopMusic(0);
                            StatusLine(s_EXIT_LEAVE);
                            StatusLine2(Concat('$', aStr),s_AND, weaponName, Concat(s_EXIT_SCORE, bStr), s_ANY);
                            StartMusic(61, $A900, 3);
                            Pause(3);
                            KeyAndShowStat;
                            gameEnded := true;
                        end else begin
                            StopMusic(0);
                            StatusLine(s_EXIT_POOR);
                            StatusLine2(s_EXIT_FATAL, s_ANY);
                            StartMusic(61, $A100, 3);
                            Pause(3);
                            repeat until keypressed;
                            gameEnded := true;
                        end;
                    end else begin
                        StopMusic(0);
                        StatusLine(s_EXIT_WRONG_PASS, currentPassword, '.');
                        StatusLine2(s_EXIT_FATAL, s_ANY);
                        StartMusic(61, $A100, 3);
                        Pause(3);
                        repeat until keypressed;
                        gameEnded := true;
                    end;

                end;

            end;

            if isIn and not gameEnded then begin // ***********   entered new room, update map
                
                MovePlayer(x + 2 * dx, y + 2 * dy);
                energy := energy - 0.5;
                stepFinished := true;
            end;


        end else begin  // **********************************   hit the wall
            StatusLine(s_BUMP);
            StatusLine2(s_NO_PASARAN, s_ANY);
            energy := energy - 0.5;
            KeyAndShowStat;
        end;

    end;

    if stepFinished and not gameEnded then begin  // ********************  Random events

        r := Random(40)+3;
        if r < consecutiveWaits then begin
            StatusLine(s_BACK_TO_START, s_ANY);
            MovePlayer(6, 1);
            repeat until keypressed;
            ShowStats;
        end else begin
            r := Random(15);
            itemLost := TILE_EMPTY_SLOT;
            case r of
                0,1,2,3,4,5,6,7:
                   if hasItem(itemSymbols[r]) then begin
                        itemLost := itemSymbols[r];
                   end;
                8: if Random(10) >= 5 then begin
                        FoundPassword;
                        skipMonster := true;
                   end;
                9,10:
                    begin
                        if weapon > 1 then begin
                            StatusLine(s_BROKE, weaponName, '.');
                            StartEffect(61, $A300, 3);
                            weapon := weapon - 4;
                            if weapon < 1 then weapon := 1;
                            weaponName := weapons[weapon - 1];
                            StatusLine2(s_FOUND, weaponName, '.', s_ANY);
                            repeat until keypressed;
                            ShowStats;
                        end;
                    end;
            end;

            if itemLost <> TILE_EMPTY_SLOT then begin
                StatusLine(s_ITEM_BROKE[r]);
                StatusLine2(s_DROPPED, s_ANY);
                StartEffect(61, $A300, 3);
                DelItem(itemLost);
                repeat until keypressed;
                ShowStats;
            end;
        end;
    
    end;

    if not skipMonster and not gameEnded then begin
        r := Random(4);
        if r>0 then begin  // ********************************** encounter !!!
            monster := GetMonster;
            
            if needPostfix(monster) then bStr := s_YOU_F
            else bStr := s_YOU_M;
            StatusLine(s_ATTACKED, bStr, monsters[monster], s_MONSTER_STR, formatFloat(monsterStrength));
            stepFinished := false;
            repeat

                if ((strength = 0) or (wounds > 4)) and (gold = 0) then begin
                    StatusLine2(s_TOO_WEAK_POOR, s_ANY);
                    repeat until keypressed;
                    StatusLine(s_BACK_TO_START, s_ANY);
                    MovePlayer(6, 1);
                    StartEffect(61, $A000, 3);
                    stepFinished := true;
                    wounds := 0;
                    KeyAndShowStat;
                end else begin
                    if (strength = 0) or (wounds > 4) then begin // ***********     too weak ?
                        StatusLine2(s_TOO_WEAK, s_ANY);
                        keycode := k_RANSOM;
                        StartEffect(61, $A500, 3);
                        KeyAndShowStat;
                    end else if gold = 0 then begin             // ************** no gold ?
                        StatusLine2(s_TOO_POOR, s_ANY);
                        keycode := k_FIGHT;
                        StartEffect(61, $A500, 3);
                        KeyAndShowStat;
                    end else begin
                        // StatusLine2(s_FIGHT, s_OR, s_RANSOM, ' ?');
                        Position(10, 28); Write(s_FIGHT, s_OR, s_RANSOM, ' ?');
                        keycode := getKey(k_FIGHT, k_RANSOM);
                    end;
                end;

                if keycode = k_FIGHT then begin  // ************** fight choosen
                    // get hurt
                    if (strength < monsterStrength * 1.2) and (Random(10) >= 4) then
                        wounds := wounds + 1;
                    // hit monster
                    monsterStrength := round(monsterStrength - ((Random * 2) + 1) * strength * 0.57);

                    ShowStats;

                    if (monsterStrength <= 0) or (Random(10) >= 5) then begin // ********* monster killed
                        r := Random(5) + 1;  // loot size
                        if needPostfix(monster) then bStr:=s_DEFEATED_F
                        else bStr:=s_DEFEATED_M;
                        StatusLine(monsters[monster], s_HAS_BEEN, bStr);
                        StatusLine2(s_EARNED, treasures[r - 1], s_ANY);
                        gold := gold + round(r * (1.25 * (1 + monsterSize / 15) + Random));
                        stepFinished := true;
                        KeyAndShowStat;
                    end else begin
                        StatusLine(monsters[monster], s_HAS_STR, formatFloat(monsterStrength));
                    end;
                end else  PayRansom // ************** pay ranson
            until stepFinished;

        end;

        GetLoot;

    end;

    if not gameEnded then begin // *********************************** use items
        stepFinished := false;
        if HasAnythingToUse then
            repeat
                StatusLine(s_WANNA_USE);
                keycode := getKey(k_YES, k_NO);
                if keycode = k_YES then begin
                    StatusLine(s_WHICH);
                    keycode:=0;
                    repeat
                        if keypressed then keycode := getKey;
                        pause;
                    until (keycode > 65) and (keycode < 90);
                    case keycode of
                        k_HAMMER:    i:=TILE_HAMMER;
                        k_LANTERN:   i:=TILE_LANTERN;
                        k_KEY:       i:=TILE_KEY;
                        k_PLANK:     i:=TILE_PLANK;
                        k_FOOD:      i:=TILE_FOOD;
                        k_DRINK:     i:=TILE_DRINK;
                        k_BANDAGE:   i:=TILE_BANDAGES;
                        k_MEDICINES: i:=TILE_MEDICINES;
                    end;

                    if not hasItem(i) then begin
                        StatusLine2(s_DONT_HAVE, chr(i), s_ANY);
                        StartEffect(61, $A500, 3);
                        repeat until keypressed;
                    end else
                        if (keycode = k_FOOD) or (keycode = k_DRINK) or (keycode = k_BANDAGE) or (keycode = k_MEDICINES) then begin
                            StartEffect(61, $A800, 3);
                            DelItem(i);
                            case keycode of
                                k_FOOD:      energy := energy + 3;
                                k_DRINK:     energy := energy + 1;
                                k_BANDAGE:   wounds := wounds - 1;
                                k_MEDICINES: wounds := wounds - 3;
                            end;
                            if wounds < 0 then wounds := 0;
                            ShowStats;
                            if Random(10)>=6 then stepFinished := true;
                        end else begin
                            Position(2,28);
                            Write(s_CAN_USE_ONLY, char(itemSymbols[4]),' ', char(itemSymbols[5]), ' ', char(itemSymbols[6]),' ', char(itemSymbols[7]), s_ANY);
                            StartEffect(61, $A500, 3);
                            repeat until keypressed;
                        end;
                end else stepFinished := true;
            until stepFinished or not HasAnythingToUse;
    end;
    Inc(moves);
end;


// *********************************** MAIN PROGRAM

begin
    fadeOut;

    veraGraphInit;
    
    veraDirectLoadPalette('assets/default.pal');
    zsmInit(50);
    zsmDirectLoad('assets/intro.zsm', 51, $A000);
    zsmDirectLoad('assets/theme.zsm', 55, $A000);
    zsmDirectLoad('assets/effect1.zsm', 61, $A000);
    zsmDirectLoad('assets/effect2.zsm', 61, $A100);     // bad ending
    zsmDirectLoad('assets/effect3.zsm', 61, $A300);     // item lost / broken weapon
    zsmDirectLoad('assets/effect4.zsm', 61, $A400);     // password found
    zsmDirectLoad('assets/effect5.zsm', 61, $A500);     // info
    zsmDirectLoad('assets/effect6.zsm', 61, $A600);     // item found
    zsmDirectLoad('assets/effect7.zsm', 61, $A700);     // weapon found
    zsmDirectLoad('assets/effect8.zsm', 61, $A800);     // used consumable
    zsmDirectLoad('assets/effect9.zsm', 61, $A900);     // good ending
    zsmSetISR;

    Randomize;
    write(X16_ISO_ON);
    TextCharset('assets/om_iso_manual.fnt');
    music:=true;
    repeat
        if music then StartMusic(51, $A000, 0);

        TitleScreen;
        ShowManual;

        StopMusic(0);
        PaintBoard;
        if music then StartMusic(55, $A000, 0);

        gameEnded := false;
        while not gameEnded do MakeMove;
    until false;

    // zsmClearISR;
    // write(X16_SWAP_GFXCHARSET);
    // TextMode(x16_MODE_80x60);
end.
