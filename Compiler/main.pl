:- use_module(dcob2cob).
:- use_module(cob2swi).

tcob2swi(File) :- atom_codes(File, FullCodes),
   atom_codes('.tcob',S),
   append(FileCodes, S ,FullCodes),
   atom_codes(Prefix,FileCodes),
   atom_concat(Prefix, '.cob', CobOutput),
   tcob2cob(File),!,cob2swi(CobOutput).
   
dcob2swi(File) :- atom_codes(File, FullCodes),
   atom_codes('.dcob',S),
   append(FileCodes, S ,FullCodes),
   atom_codes(Prefix,FileCodes),
   atom_concat(Prefix, '.cob', CobOutput),
   dcob2cob(File),!,cob2swi(CobOutput).
