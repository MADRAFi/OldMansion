
const
    monsters: array [0..MONSTERS_COUNT - 1] of TString = (
        'Bat',
        'Rat',
        'Lizard',
        'Spider',
        'Dog',
        'Snake',
        'Kronk',
        'Gnome',
        'Goblin',
        'Zombie',
        'Warg',
        'Ghost',
        'Elf',
        'Werewolf',
        'Witch',
        'Orc',
        'Gargoyle',
        'Troll',
        'Mage',
        'Vampire',
        'Nightmare',
        'Minotaur',
        'Cyclope',
        'Giant',
        'Hydra',
        'Demon',
        'Medusa',
        'Dragon',
        'Devil',
        'Phoenix'
     );
    weapons: array [0..WEAPONS_COUNT - 1] of TString = (
        'Fork',
        'Knife',
        'Razor',
        'Dagger',
        'Sabber',
        'Lance',
        'Javelin',
        'Scimitar',
        'Axe',
        'Sword' 
    );
    treasures: array [0..TREASURES_COUNT - 1] of TString = (
        'copper',
        'silver',
        'gold',
        'platinum',
        'diamonds' 
    );
    items: array [0..ITEMS_COUNT - 1] of TString = (        
        'HAMMER',
        'LANTERN',
        'KEY',
        'PLANK',
        'FOOD',
        'DRINK',
        'BANDAGE',
        'MEDICINE',
        'PASSWORD' 
    );
    // itemSymbols: array [0..ITEMS_COUNT - 2] of char = ( 'H', 'L', 'K', 'P', 'F', 'D', 'B', 'M');
    itemSymbols: array [0..ITEMS_COUNT - 2] of byte = ( TILE_HAMMER, TILE_LANTERN, TILE_KEY, TILE_PLANK, TILE_FOOD, TILE_DRINK, TILE_BANDAGES, TILE_MEDICINES);
    passwords: array [0..PASSWORDS_COUNT - 1] of TString = ('FISH','EGGS','BYTE','TREE','CLAN','CULT','MOON','BULB','LASH');

const
    k_F10 = $15;
    k_YES = byte('Y');
    k_NO = byte('N');
    k_LEFT = byte('L');
    k_RIGHT = byte('R');
    k_UP = byte('U');
    k_DOWN = byte('D');
    k_FIGHT = byte('F');
    k_RANSOM = byte('B');
    k_TAKE = byte('T');
    k_LEAVE = byte('L');
    k_REST = byte('R');
    k_MOVE = byte('M');

    k_HAMMER = byte('H');
    k_LANTERN = byte('L');
    k_KEY = byte('K');
    k_PLANK = byte('P');
    k_FOOD = byte('F');
    k_DRINK = byte('D');
    k_BANDAGE = byte('B');
    k_MEDICINES = byte('M');

    s_PRESS_ANY: string = 'PRESS ANY KEY';
    s_WANT_MANUAL: string = 'Do you want to read a manual '#187#185#188'/'#187#174#188'?';
    s_ENERGY: string = #187#165#174#165#178#167#185#188;
    s_TREASURE: string = #187#180#178#165#161#179#181#178#165#188;
    s_MOVES: string = #187#173#175#182#165#179#188;
    s_WOUNDS: string = #187#183#175#181#174#164#179#188;
    s_ITEMS: string = #187#169#180#165#173#179#188;
    s_WEAPON: string = #187#183#165#161#176#175#174#188;
    s_ATTACK: string = #187#161#180#180#161#163#171#188;
    s_FOUND: string = 'You have found ';
    s_TAKE: string = #187#180#188'ake';
    s_OR: string = '  or ';
    s_LEAVE: string = #187#172#188'eave';
    s_FOUND_PASS: string = 'You''ve found password: ';
    s_REMEMBER: string = 'Remember it.';
    s_LEAVE_WHAT: string = 'What do you want to drop?   ';
    s_DONT_HAVE: string = 'You don''t have ';
    s_WAIT: string = #187#178#188'est';
    s_MOVE: string = #187#173#188'ove';
    s_LEFT: string = #187#172#188'eft';
    s_RIGHT: string = #187#178#188'ight';
    s_UP: string = #187#181#188'p';
    s_DOWN: string = #187#164#188'own';
    s_DROPPED: string = 'So you dropped it.';
    s_USED: string = 'You''ve used ';
    s_ATTACKED: string = 'Attacked';
    s_DOOR_OPENED: string = 'The door is open, entering.';
    s_DOOR_CLOSED: string = 'Door is closed, ';
    s_KEY: string = 'KEY';
    s_BYKEY: string = 'KEY';
    s_WALL: string = 'Thin Wall.';
    s_HAMMER: string = 'HAMMER';
    s_BYHAMMER: string = 'HAMMER';
    s_ROOM_DARK: string = 'The room is dark.';
    s_LANTERN: string = 'LANTERN';
    s_BYLANTERN: string = 'LANTERN';
    s_ROOM_HOLE: string = 'There is no floor in this room.';
    s_PLANK: string = 'PLANK';
    s_BYPLANK: string = 'PLANK';
    s_EXIT_PASS: string = 'You''ve reached an Exit. Say PASSWORD.';
    s_EXIT_PAY: string = 'Thank You! Now please pay $100.';
    s_EXIT_LEAVE: string = 'You''ve left the OLD MANSION with';
    s_EXIT_SCORE: string = '. SCORE = ';
    s_EXIT_POOR: string = 'Ouch! You don''t have enough money.';
    s_EXIT_FATAL: string = 'Guardian cuts your head off.';
    s_EXIT_WRONG_PASS: string = 'Wrong! Password was: ';
    s_BUMP: string = 'You hit the wall';
    s_NO_PASARAN: string = 'Better find other way.';
    s_BACK_TO_START: string = 'You''ve been teleported to Start.';
    s_ANY: string = #187#161#174#185#188;
    s_ITEM_BROKE: array [0..7] of string = (
        'Hammer is broken, ',
        'Lantern is out of oil, ',
        'Key is rusted, ',
        'Plank decayed, ',
        'Food is rotten, ',
        'Drink dried off, ',
        'Bandage is ripped, ',
        'Medicine has expired, '
    );
    s_BROKE: string = 'You''ve broke ';
    s_YOU_M: string = ' by ';
    s_YOU_F: string = '';
    s_MONSTER_STR: string = '. Attack ';
    s_TOO_WEAK_POOR: string = 'You are too weak and poor';
    s_TOO_WEAK: string = 'You are too weak, You must pay.';
    s_TOO_POOR: string = 'You have to fight.';
    s_FIGHT: string = #187#166#188'ight';
    s_RANSOM: string = #187#162#188'ribe';
    s_HAS_BEEN: string = ' has been';
    s_DEFEATED_F: string = '';
    s_DEFEATED_M: string = ' defeated.';
    s_EARNED: string = 'You earn ';
    s_HAS_STR: string = ' has attack ';
    s_WANNA_USE: string = 'Do you want to use an item '#187#185#188'/'#187#174#188'?';
    s_WHICH: string = 'Which one?       ';
    s_CAN_USE_ONLY: string = 'You can use only ';
    s_AND: string = ' and ';
    
    
function needPostfix(monster: byte): boolean;
begin
    result := false; 
end;

procedure ManualPage_C80;
begin
    TextBackground(X16_COLOR_BLACK);
    TextColor(X16_COLOR_WHITE);
    Position(2,3);
    Print('You have suddenly found yourself in an old and spooky mansion.');
    Position(2,5);
    Print('You need to get out of here. There is only one exit and it''s guarded.');
    Position(2,7);
    Print('The guard will let you out if you tell him the password and pay $100.');
    Position(2,9);
    Print('You need to collect the money (by finding the treasures) which belong to');
    Position(2,11);
    Print('the monsters living here. You can fight them if you have less than 5 wounds');
    Position(2,13);
    Print('and your power is greater than 0. You can increase your energy when you');
    Position(2,15);
    Print('find or buy food or drinks. Wounds can be healed with medicines or bandages.');
    Position(2,17);
    Print('If you can''t fight, you need to pay the monster for letting you out.');
    Position(2,19);
    Print('The password can be found somewhere in the mansion. Memorise it well!');
    Position(2,21);
    Print('The mansion is very old. Some rooms are dark, some have no floor.');
    Position(2,23);
    Print('You can find closed doors or thin walls so make sure to collect items.');
    Position(2,25);
    Print('But remember - you can only carry four items and one weapon at once.');
    Position(2,27);
    Print('Pick always the best weapon; it depends on your attack strength and');
    Position(2,29);
    Print('the treasures you''ve collected.');

    Position(4,32);
    Print('SYMBOLS:');
    Position(5,34);
    Print(char(TILE_DOOR_H), #32); Print(char(TILE_DOOR_V)); Print( ' - closed doors');
    Position(5,36);
    Print(char(TILE_WALL_H), #32); Print(char(TILE_WALL_V)); Print( ' - thin wall');
    Position(5,38);
    Print(char(TILE_HOLE)); Print(' - collapsed floor');
    Position(5,40);
    Print(char(TILE_DARK)); Print(' - dark room');
    Position(5,42);
    Print(char(TILE_HAMMER)); Print(' - hammer');
    Position(5,44);
    Print(char(TILE_LANTERN)); Print(' - lantern');
    Position(5,46);
    Print(char(TILE_KEY)); Print(' - key');
    Position(5,48);
    Print(char(TILE_PLANK)); Print(' - plank');
    Position(5,50);
    Print(chr(TILE_EXIT),chr(TILE_EXIT1)); Print(' - exit');

    Position(21,42);
    Print(char(TILE_FOOD)); Print(' - food');
    Position(21,44);
    Print(char(TILE_DRINK)); Print(' - drinks');
    Position(21,46);
    Print(char(TILE_BANDAGES)); Print(' - bandages');
    Position(21,48);
    Print(char(TILE_MEDICINES)); Print(' - medicines');
    Position(21,50);
    Print(char(TILE_PLAYER),' - this is you');
    
    Position(45,32);
    Print('KEYBINDS:');
    Position(45,34);
    Print(s_ANY, ' - press any key');
    Position(45,36);
    Print(#187#185#188, ' - yes');
    Position(45,38);
    Print(#187#174#188, ' - no');
    Position(45,40);
    Print(#187#178#188, ' - rest');
    Position(45,42);
    Print(#187#173#188, ' - move');
    Position(45,44);
    Print(#187#166#188, ' - fight');
    Position(45,46);
    Print(#187#162#188, ' - bribe');
    Position(45,48);
    Print(#187#180#188, ' - take');
    Position(45,50);
    Print(#187#172#188, ' - leave');

    Position(63,44);
    Print(#187#172#188, ' - left');
    Position(63,46);
    Print(#187#178#188, ' - right');
    Position(63,48);
    Print(#187#181#188, ' - up');
    Position(63,50);
    Print(#187#164#188, ' - down');

end;