
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.hpp"
#include "cgen_gc.hpp"

#include <algorithm>
#include <functional>
#include <queue>
#include <stack>

extern void emit_string_constant(ostream& str, char* s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cpp) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string, IO, length, Main, main_meth, No_class,
    No_type, Object, out_int, out_string, prim_slot, self, SELF_TYPE, Str, str_field, substr, type_name, val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg = idtable.add_string("arg");
    arg2 = idtable.add_string("arg2");
    Bool = idtable.add_string("Bool");
    concat = idtable.add_string("concat");
    cool_abort = idtable.add_string("abort");
    copy = idtable.add_string("copy");
    Int = idtable.add_string("Int");
    in_int = idtable.add_string("in_int");
    in_string = idtable.add_string("in_string");
    IO = idtable.add_string("IO");
    length = idtable.add_string("length");
    Main = idtable.add_string("Main");
    main_meth = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any
    //   user-defined class.
    No_class = idtable.add_string("_no_class");
    No_type = idtable.add_string("_no_type");
    Object = idtable.add_string("Object");
    out_int = idtable.add_string("out_int");
    out_string = idtable.add_string("out_string");
    prim_slot = idtable.add_string("_prim_slot");
    self = idtable.add_string("self");
    SELF_TYPE = idtable.add_string("SELF_TYPE");
    Str = idtable.add_string("String");
    str_field = idtable.add_string("_str_field");
    substr = idtable.add_string("substr");
    type_name = idtable.add_string("type_name");
    val = idtable.add_string("_val");
}

static char* gc_init_names[] = {"_NoGC_Init", "_GenGC_Init", "_ScnGC_Init"};
static char* gc_collect_names[] = {"_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect"};

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cpp'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream& os)
{
    // spim wants comments to start with '#'
    os << "# start of generated code\n";

    initialize_constants();
    CgenClassTable* codegen_classtable = new CgenClassTable(classes, os);

    os << "\n# end of generated code\n";
}

//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.hpp)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.hpp'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char* dest_reg, int offset, char* source_reg, ostream& s)
{
    s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" << endl;
}

static void emit_store(char* source_reg, int offset, char* dest_reg, ostream& s)
{
    s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")" << endl;
}

static void emit_load_imm(char* dest_reg, int val, ostream& s)
{
    s << LI << dest_reg << " " << val << endl;
}

static void emit_load_address(char* dest_reg, char* address, ostream& s)
{
    s << LA << dest_reg << " " << address << endl;
}

static void emit_partial_load_address(char* dest_reg, ostream& s)
{
    s << LA << dest_reg << " ";
}

static void emit_load_bool(char* dest, const BoolConst& b, ostream& s)
{
    emit_partial_load_address(dest, s);
    b.code_ref(s);
    s << endl;
}

static void emit_load_string(char* dest, StringEntry* str, ostream& s)
{
    emit_partial_load_address(dest, s);
    str->code_ref(s);
    s << endl;
}

static void emit_load_int(char* dest, IntEntry* i, ostream& s)
{
    emit_partial_load_address(dest, s);
    i->code_ref(s);
    s << endl;
}

static void emit_move(char* dest_reg, char* source_reg, ostream& s)
{
    s << MOVE << dest_reg << " " << source_reg << endl;
}

static void emit_neg(char* dest, char* src1, ostream& s)
{
    s << NEG << dest << " " << src1 << endl;
}

static void emit_add(char* dest, char* src1, char* src2, ostream& s)
{
    s << ADD << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addu(char* dest, char* src1, char* src2, ostream& s)
{
    s << ADDU << dest << " " << src1 << " " << src2 << endl;
}

static void emit_addiu(char* dest, char* src1, int imm, ostream& s)
{
    s << ADDIU << dest << " " << src1 << " " << imm << endl;
}

static void emit_div(char* dest, char* src1, char* src2, ostream& s)
{
    s << DIV << dest << " " << src1 << " " << src2 << endl;
}

static void emit_mul(char* dest, char* src1, char* src2, ostream& s)
{
    s << MUL << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sub(char* dest, char* src1, char* src2, ostream& s)
{
    s << SUB << dest << " " << src1 << " " << src2 << endl;
}

static void emit_sll(char* dest, char* src1, int num, ostream& s)
{
    s << SLL << dest << " " << src1 << " " << num << endl;
}

static void emit_jalr(char* dest, ostream& s)
{
    s << JALR << "\t" << dest << endl;
}

static void emit_jal(char* address, ostream& s)
{
    s << JAL << address << endl;
}

static void emit_jal_raw(ostream& s)
{
    s << JAL;
}

static void emit_return(ostream& s)
{
    s << RET << endl;
}

static void emit_gc_assign(ostream& s)
{
    s << JAL << "_GenGC_Assign" << endl;
}

static void emit_disptable_ref(Symbol sym, ostream& s)
{
    s << sym << DISPTAB_SUFFIX;
}

static void emit_init_ref(Symbol sym, ostream& s)
{
    s << sym << CLASSINIT_SUFFIX;
}

static void emit_label_ref(int l, ostream& s)
{
    s << "label" << l;
}

static void emit_jump(int l, ostream& s)
{
    s << JUMP;
    emit_label_ref(l, s);
    s << endl;
}

static void emit_protobj_ref(Symbol sym, ostream& s)
{
    s << sym << PROTOBJ_SUFFIX;
}

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{
    s << classname << METHOD_SEP << methodname;
}

static void emit_method_def(Symbol classname, Symbol methodname, ostream& s)
{
    s << classname << METHOD_SEP << methodname << ":" << endl;
}

static void emit_label_def(int l, ostream& s)
{
    emit_label_ref(l, s);
    s << ":" << endl;
}

static void emit_beqz(char* source, int label, ostream& s)
{
    s << BEQZ << source << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_beq(char* src1, char* src2, int label, ostream& s)
{
    s << BEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bne(char* src1, char* src2, int label, ostream& s)
{
    s << BNE << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bleq(char* src1, char* src2, int label, ostream& s)
{
    s << BLEQ << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blt(char* src1, char* src2, int label, ostream& s)
{
    s << BLT << src1 << " " << src2 << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_blti(char* src1, int imm, int label, ostream& s)
{
    s << BLT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_bgti(char* src1, int imm, int label, ostream& s)
{
    s << BGT << src1 << " " << imm << " ";
    emit_label_ref(label, s);
    s << endl;
}

static void emit_branch(int l, ostream& s)
{
    s << BRANCH;
    emit_label_ref(l, s);
    s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char* reg, ostream& str)
{
    emit_store(reg, 0, SP, str);
    emit_addiu(SP, SP, -4, str);
}

static void emit_pop(char* reg, ostream& str)
{
    emit_addiu(SP, SP, 4, str);
    emit_load(reg, 0, SP, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char* dest, char* source, ostream& s)
{
    emit_load(dest, DEFAULT_OBJFIELDS, source, s);
}

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char* source, char* dest, ostream& s)
{
    emit_store(source, DEFAULT_OBJFIELDS, dest, s);
}

static void emit_test_collector(ostream& s)
{
    emit_push(ACC, s);
    emit_move(ACC, SP, s);  // stack end
    emit_move(A1, ZERO, s); // allocate nothing
    s << JAL << gc_collect_names[cgen_Memmgr] << endl;
    emit_addiu(SP, SP, 4, s);
    emit_load(ACC, 0, SP, s);
}

static void emit_gc_check(char* source, ostream& s)
{
    if (source != (char*)A1)
        emit_move(A1, source, s);
    s << JAL << "_gc_check" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
    s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                                              // label
      << WORD << stringclasstag << endl                                     // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len + 4) / 4) << endl // size
      << WORD;
    emit_disptable_ref(Str, s);
    s << endl;
    s << WORD;
    lensym->code_ref(s);
    s << endl;                    // string length
    emit_string_constant(s, str); // ascii string
    s << ALIGN;                   // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{
    for (List<StringEntry>* l = tbl; l; l = l->tl())
        l->hd()->code_def(s, stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream& s)
{
    s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream& s, int intclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                           // label
      << WORD << intclasstag << endl                     // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl // object size
      << WORD;
    emit_disptable_ref(Int, s);
    s << endl;

    s << WORD << str << endl; // integer value
}

//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream& s, int intclasstag)
{
    for (List<IntEntry>* l = tbl; l; l = l->tl())
        l->hd()->code_def(s, intclasstag);
}

//
// Bools
//
BoolConst::BoolConst(int i)
    : val(i)
{
    assert(i == 0 || i == 1);
}

void BoolConst::code_ref(ostream& s) const
{
    s << BOOLCONST_PREFIX << val;
}

//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);
    s << LABEL                                            // label
      << WORD << boolclasstag << endl                     // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl // object size
      << WORD;
    emit_disptable_ref(Bool, s);
    s << endl;

    s << WORD << val << endl; // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
    Symbol main = idtable.lookup_string(MAINNAME);
    Symbol string = idtable.lookup_string(STRINGNAME);
    Symbol integer = idtable.lookup_string(INTNAME);
    Symbol boolc = idtable.lookup_string(BOOLNAME);

    str << "\t.data\n" << ALIGN;
    //
    // The following global names must be defined first.
    //
    str << GLOBAL << CLASSNAMETAB << endl;
    str << GLOBAL;
    emit_protobj_ref(main, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(integer, str);
    str << endl;
    str << GLOBAL;
    emit_protobj_ref(string, str);
    str << endl;
    str << GLOBAL;
    falsebool.code_ref(str);
    str << endl;
    str << GLOBAL;
    truebool.code_ref(str);
    str << endl;
    str << GLOBAL << INTTAG << endl;
    str << GLOBAL << BOOLTAG << endl;
    str << GLOBAL << STRINGTAG << endl;

    //
    // We also need to know the tag of the Int, String, and Bool classes
    // during code generation.
    //
    str << INTTAG << LABEL << WORD << intclasstag << endl;
    str << BOOLTAG << LABEL << WORD << boolclasstag << endl;
    str << STRINGTAG << LABEL << WORD << stringclasstag << endl;
}

//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
    str << GLOBAL << HEAP_START << endl << HEAP_START << LABEL << WORD << 0 << endl << "\t.text" << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Main"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Int"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("String"), str);
    str << endl << GLOBAL;
    emit_init_ref(idtable.add_string("Bool"), str);
    str << endl << GLOBAL;
    emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
    str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
    falsebool.code_def(str, boolclasstag);
    truebool.code_def(str, boolclasstag);
}

void CgenClassTable::code_select_gc()
{
    //
    // Generate GC choice constants (pointers to GC functions)
    //
    str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
    str << "_MemMgr_INITIALIZER:" << endl;
    str << WORD << gc_init_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
    str << "_MemMgr_COLLECTOR:" << endl;
    str << WORD << gc_collect_names[cgen_Memmgr] << endl;
    str << GLOBAL << "_MemMgr_TEST" << endl;
    str << "_MemMgr_TEST:" << endl;
    str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}

//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
    //
    // Add constants that are required by the code generator.
    //
    stringtable.add_string("");
    inttable.add_string("0");

    stringtable.code_string_table(str, stringclasstag);
    inttable.code_string_table(str, intclasstag);
    code_bools(boolclasstag);
}

void CgenClassTable::code_prototype_objects()
{
    apply_in_tag_order([](CgenNodeP node, int tag, ostream& str) { node->code_prototype_object(tag, str); });
}

void CgenClassTable::code_class_name_table()
{
    str << CLASSNAMETAB << ":" << endl;
    apply_in_tag_order([](CgenNodeP node, int tag, ostream& str) { node->code_class_name_entry(tag, str); });
    str << std::endl;
}

void CgenClassTable::code_dispatch_table()
{
    apply_in_tag_order([](CgenNodeP node, int tag, ostream& str) { node->code_dispatch_entry(tag, str); });
}

void CgenClassTable::code_class_obj_table()
{
    str << CLASSOBJTAB << ":" << endl;
    apply_in_tag_order([](CgenNodeP node, int tag, ostream& str) { node->code_class_obj_entry(tag, str); });
    str << std::endl;
}

void CgenNode::code_class_obj_entry(int tag, ostream& str)
{
    str << WORD;
    emit_protobj_ref(name, str);
    str << endl;
    str << WORD;
    emit_init_ref(name, str);
    str << endl;
}

CgenClassTable::CgenClassTable(Classes classes, ostream& s)
    : str(s)
    , predefined_method_classes {Object, IO, Str}
{
    stringclasstag = 4;
    intclasstag = 2;
    boolclasstag = 3;

    if (cgen_debug)
    {
        std::cout << "Building CgenClassTable" << std::endl;
    }
    install_basic_classes();
    install_classes(classes);

    build_inheritance_tree();

    if (cgen_debug)
    {
        std::cout << "Constructing attribute tables" << std::endl;
    }
    construct_attribute_tables();

    if (cgen_debug)
    {
        std::cout << "Constructing dispatch tables" << std::endl;
    }
    construct_dispatch_tables();

    code();
}

void CgenClassTable::install_basic_classes()
{

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    //
    // A few special class names are installed in the lookup table but not
    // the class list.  Thus, these classes exist, but are not part of the
    // inheritance hierarchy.
    // No_class serves as the parent of Object and the other special classes.
    // SELF_TYPE is the self class; it cannot be redefined or inherited.
    // prim_slot is a class known to the code generator.
    //
    add_special_class(No_class, new CgenNode(class_(No_class, No_class, nil_Features(), filename), Basic, this));
    add_special_class(SELF_TYPE, new CgenNode(class_(SELF_TYPE, No_class, nil_Features(), filename), Basic, this));
    add_special_class(prim_slot, new CgenNode(class_(prim_slot, No_class, nil_Features(), filename), Basic, this));

    //
    // The Object class has no parent class. Its methods are
    //        cool_abort() : Object    aborts the program
    //        type_name() : Str        returns a string representation of class
    //        name copy() : SELF_TYPE       returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.
    //
    install_class(new CgenNode(
        class_(
            Object,
            No_class,
            append_Features(
                append_Features(
                    single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                    single_Features(method(type_name, nil_Formals(), Str, no_expr()))
                ),
                single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))
            ),
            filename
        ),
        Basic,
        this
    ));

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE          writes a string to the output
    //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
    //        in_string() : Str                    reads a string from the input
    //        in_int() : Int                         "   an int     "  "     "
    //
    install_class(new CgenNode(
        class_(
            IO,
            Object,
            append_Features(
                append_Features(
                    append_Features(
                        single_Features(method(out_string, single_Formals(formal(arg, Str)), SELF_TYPE, no_expr())),
                        single_Features(method(out_int, single_Formals(formal(arg, Int)), SELF_TYPE, no_expr()))
                    ),
                    single_Features(method(in_string, nil_Formals(), Str, no_expr()))
                ),
                single_Features(method(in_int, nil_Formals(), Int, no_expr()))
            ),
            filename
        ),
        Basic,
        this
    ));

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    install_class(
        new CgenNode(class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())), filename), Basic, this)
    );

    //
    // Bool also has only the "val" slot.
    //
    install_class(
        new CgenNode(class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename), Basic, this)
    );

    //
    // The class Str has a number of slots and operations:
    //       val                                  ???
    //       str_field                            the string itself
    //       length() : Int                       length of the string
    //       concat(arg: Str) : Str               string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring
    //
    install_class(new CgenNode(
        class_(
            Str,
            Object,
            append_Features(
                append_Features(
                    append_Features(
                        append_Features(
                            single_Features(attr(val, Int, no_expr())),
                            single_Features(attr(str_field, prim_slot, no_expr()))
                        ),
                        single_Features(method(length, nil_Formals(), Int, no_expr()))
                    ),
                    single_Features(method(concat, single_Formals(formal(arg, Str)), Str, no_expr()))
                ),
                single_Features(method(
                    substr,
                    append_Formals(single_Formals(formal(arg, Int)), single_Formals(formal(arg2, Int))),
                    Str,
                    no_expr()
                ))
            ),
            filename
        ),
        Basic,
        this
    ));
}

void CgenClassTable::add_special_class(Symbol name, CgenNodeP nd)
{
    if (special_classes_map.find(name) != special_classes_map.end())
    {
        throw std::runtime_error(std::string("Special class already added: ") + name->get_string());
    }

    special_classes_map[name] = nd;
}

CgenNodeP CgenClassTable::get_special_node_from_symbol(Symbol name) const
{
    auto it = special_classes_map.find(name);
    if (it != special_classes_map.end())
    {
        return it->second;
    }
    return nullptr;
}

CgenNodeP CgenClassTable::get_normal_node_from_symbol(Symbol name) const
{
    auto it = class_tag_map.find(name);
    if (it != class_tag_map.end())
    {
        return class_nodes[it->second];
    }
    return nullptr;
}

int CgenClassTable::get_tag_from_symbol(Symbol name) const
{
    auto it = class_tag_map.find(name);
    if (it != class_tag_map.end())
    {
        return it->second;
    }
    return -1;
}

CgenNodeP CgenClassTable::get_node_from_tag(int tag) const
{
    if (tag >= 0 && tag < class_nodes.size())
    {
        return class_nodes[tag];
    }
    return nullptr;
}

bool CgenClassTable::is_special_symbol(Symbol name) const
{
    return get_special_node_from_symbol(name) != nullptr;
}

CgenNodeP CgenClassTable::get_node_from_symbol(Symbol name) const
{
    auto nd = get_normal_node_from_symbol(name);
    if (nd != nullptr)
    {
        return nd;
    }
    return get_special_node_from_symbol(name);
}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
    Symbol name = nd->get_name();

    class_tag_map[name] = next_tag++;
    class_nodes.push_back(nd);
}

void CgenClassTable::install_classes(Classes cs)
{
    for (int i = cs->first(); cs->more(i); i = cs->next(i))
    {
        install_class(new CgenNode(cs->nth(i), NotBasic, this));
    }
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
    for (auto node : class_nodes)
    {
        set_relations(node);
    }

    if (cgen_debug)
    {
        for (auto node : class_nodes)
        {
            std::cout << node->get_name()->get_string() << " has parent " << node->get_parent()->get_string() << " ";
            std::cout << " and children: " << std::endl;
            for (auto child : node->get_children())
            {
                std::cout << "  " << child->get_name()->get_string() << endl;
            }
        }
    }
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
    auto parent_sym = nd->get_parent();
    CgenNodeP parent_node = get_node_from_symbol(parent_sym);
    nd->set_parentnd(parent_node);
    parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
    children.push_back(n);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
    assert(parentnd == NULL);
    assert(p != NULL);
    parentnd = p;
}

void CgenClassTable::construct_attribute_tables()
{
    // We need to start applying from root to children.
    // This is to make sure each child has the parent's attributes available.
    apply_from_root([](CgenNodeP node) { node->construct_attribute_table(); });
}

void CgenClassTable::construct_dispatch_tables()
{
    // We need to start applying from root to children.
    // This is to make sure each child has the parent's methods available.
    apply_from_root([](CgenNodeP node) { node->construct_dispatch_table(); });
}

void CgenClassTable::apply_from_root(std::function<void(CgenNodeP)> func)
{
    std::queue<CgenNodeP> queue {};
    queue.push(root());

    while (!queue.empty())
    {
        auto node = queue.front();
        queue.pop();

        func(node);

        for (auto child : node->get_children())
        {
            queue.push(child);
        }
    }
}

void CgenNode::construct_attribute_table()
{
    auto parent_nd = get_parentnd();
    attribute_table = parent_nd->get_attribute_table();
    attribute_offset_map = parent_nd->get_attribute_offset_map();

    auto i = features->first();
    while (features->more(i))
    {
        auto feature = features->nth(i);
        auto attr_ptr = dynamic_cast<attr_class*>(feature);
        if (attr_ptr == nullptr)
        {
            i = features->next(i);
            continue;
        }

        // Everything here is a new attribute.
        // No need to check if it already exists.
        // This has already been checked during semantic analysis.
        attribute_table.push_back({name, attr_ptr});
        attribute_offset_map[attr_ptr->get_name()] = attribute_table.size() - 1;
        i = features->next(i);
    }

    if (cgen_debug)
    {
        std::cout << "Attribute table for " << get_name()->get_string() << std::endl;
        for (auto attr : attribute_table)
        {
            std::cout << "  " << attr.first->get_string() << std::endl;
        }
    }
}

void CgenNode::construct_dispatch_table()
{
    auto parent_nd = get_parentnd();
    dispatch_table = parent_nd->get_dispatch_table();
    dispatch_offset_map = parent_nd->get_dispatch_offset_map();

    auto i = features->first();
    while (features->more(i))
    {

        auto feature = features->nth(i);
        auto method_ptr = dynamic_cast<method_class*>(feature);
        if (method_ptr == nullptr)
        {
            i = features->next(i);
            continue;
        }

        auto it = dispatch_offset_map.find(method_ptr->get_name());
        if (it != dispatch_offset_map.end())
        {
            // Method already exists, override it.
            dispatch_table[it->second] = {name, method_ptr};
        }
        else
        {
            // New method, add it.
            dispatch_table.push_back({name, method_ptr});
            dispatch_offset_map[method_ptr->get_name()] = dispatch_table.size() - 1;
        }
        i = features->next(i);
    }

    if (cgen_debug)
    {
        std::cout << "Dispatch table for " << get_name()->get_string() << std::endl;
        for (auto method : dispatch_table)
        {
            std::cout << "  " << method.first->get_string() << std::endl;
        }
    }
}

void CgenClassTable::apply_in_tag_order(std::function<void(CgenNodeP, int, ostream&)> func)
{
    for (size_t i = 0; i < class_nodes.size(); i++)
    {
        func(class_nodes[i], i, str);
    }
}

void CgenNode::code_prototype_object(int tag, ostream& str)
{
    str << WORD << -1 << " # For GC" << std::endl;
    emit_protobj_ref(name, str);
    str << LABEL;
    str << WORD << tag << std::endl;
    str << WORD << (DEFAULT_OBJFIELDS + attribute_table.size()) << std::endl;
    str << WORD;
    emit_disptable_ref(name, str);
    str << std::endl;

    for (auto [symbol, attr_ptr] : attribute_table)
    {
        str << WORD;
        // Need to first find the class of the attribute.
        if (attr_ptr->get_type_decl() == Int)
        {
            inttable.lookup_string("0")->code_ref(str);
        }
        else if (attr_ptr->get_type_decl() == Bool)
        {
            falsebool.code_ref(str);
        }
        else if (attr_ptr->get_type_decl() == Str)
        {
            stringtable.lookup_string("")->code_ref(str);
        }
        else
        {
            str << 0;
        }
        str << " # attribute " << attr_ptr->get_name()->get_string() << std::endl;
    }

    str << std::endl;
}

void CgenNode::code_class_name_entry(int tag, ostream& str)
{
    str << WORD;
    stringtable.lookup_string(name->get_string())->code_ref(str);
    str << " # " << name->get_string();
    str << std::endl;
}

void CgenNode::code_dispatch_entry(int tag, ostream& str)
{
    emit_disptable_ref(name, str);
    str << LABEL;
    for (auto [symbol, method_ptr] : dispatch_table)
    {
        // Here we emit the method but we take the current class name.
        // We use the class that defined the method to avoid duplicity.
        str << WORD;
        emit_method_ref(symbol, method_ptr->get_name(), str);
        str << std::endl;
    }
    str << std::endl;
}

void CgenClassTable::code_object_initializers()
{
    apply_in_tag_order([&](CgenNodeP node, int tag, ostream& str) {
        node->code_object_initializer(tag, str, this, node->get_name());
    });
}

void CgenNode::code_object_initializer(int tag, ostream& str, CgenClassTableP class_table, Symbol class_name)
{
    emit_init_ref(name, str);
    str << LABEL;

    emit_move(FP, SP, str);
    emit_push(RA, str);
    // Set SELF to the object being initialized (passed in $a0).
    // This is needed because trap.handler calls *_init directly without setting
    // $s0.
    emit_move(SELF, ACC, str);

    // Iterate by offset (declaration order), not alphabetical order!
    // This ensures attributes are initialized in the correct dependency order.
    for (size_t offset = 0; offset < attribute_table.size(); offset++)
    {
        auto [symbol, attr_ptr] = attribute_table[offset];
        Tracker locals_env {}; // Standard tracker since we only push RA (like normal
                               // methods)
        str << "# a " << symbol << endl;
        if (dynamic_cast<no_expr_class*>(attr_ptr->get_init()) == nullptr)
        {
            str << "# c " << symbol << endl;
            attr_ptr->get_init()->code(str, class_table, class_name, locals_env);

            str << "# b " << symbol << endl;
            emit_store(ACC, DEFAULT_OBJFIELDS + offset, SELF, str);
        }
    }

    emit_move(ACC, SELF, str); // Return the initialized object!
    emit_pop(RA, str);
    emit_return(str);

    str << std::endl;
}

void CgenClassTable::code_class_methods()
{
    apply_from_root([&](CgenNodeP node) {
        // Methods for these objects are in trap.handler
        if (predefined_method_classes.find(node->get_name()) != predefined_method_classes.end())
        {
            return;
        }

        node->code_class_method(str, this, node->get_name());
    });
}

void CgenNode::code_class_method(ostream& s, CgenClassTableP class_table, Symbol class_name)
{
    for (auto i = features->first(); features->more(i); i = features->next(i))
    {
        auto feature = features->nth(i);
        auto method_ptr = dynamic_cast<method_class*>(feature);
        if (method_ptr == nullptr)
        {
            continue;
        }

        Tracker locals_env {};
        locals_env.enterscope();
        std::stack<Symbol> formal_stack {};
        for (int j = method_ptr->get_formals()->first(); method_ptr->get_formals()->more(j);
             j = method_ptr->get_formals()->next(j))
        {
            formal_stack.push(method_ptr->get_formals()->nth(j)->get_name());
        }
        while (!formal_stack.empty())
        {
            locals_env.add_parameter_id(formal_stack.top());
            formal_stack.pop();
        }

        s << std::endl;
        emit_method_def(name, method_ptr->get_name(), s);
        emit_move(FP, SP, s);
        emit_push(RA, s);
        method_ptr->get_expr()->code(s, class_table, class_name, locals_env);

        locals_env.exitscope();

        emit_pop(RA, s);
        s << "# remove formals " << method_ptr->get_formals()->len() << method_ptr->get_name()->get_string()
          << std::endl;
        emit_addiu(SP, SP, method_ptr->get_formals()->len() * 4, s);
        emit_return(s);
    }
    s << std::endl;
}

void CgenClassTable::code()
{
    if (cgen_debug)
        cout << "coding global data" << endl;
    code_global_data();

    if (cgen_debug)
        cout << "choosing gc" << endl;
    code_select_gc();

    if (cgen_debug)
        cout << "coding constants" << endl;
    code_constants();

    if (cgen_debug)
        cout << "coding prototype objects" << endl;
    code_prototype_objects();

    if (cgen_debug)
        cout << "coding class name table" << endl;
    code_class_name_table();

    if (cgen_debug)
        cout << "coding dispatch tables" << endl;
    code_dispatch_table();

    if (cgen_debug)
        cout << "coding class object table" << endl;
    code_class_obj_table();

    if (cgen_debug)
        cout << "coding global text" << endl;
    code_global_text();

    if (cgen_debug)
        cout << "coding object initializer" << endl;
    code_object_initializers();

    if (cgen_debug)
        cout << "coding class methods" << endl;
    code_class_methods();

    // TODO find out how to make mips work without the below.
    str << "main:" << std::endl;
    str << "j __start" << std::endl;
}

CgenNodeP CgenClassTable::root()
{
    return get_node_from_symbol(Object);
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct)
    : class__class((const class__class&)*nd)
    , parentnd(NULL)
    , children(NULL)
    , basic_status(bstatus)
{
    stringtable.add_string(name->get_string());
}

//////////////////////////////////////////////////////////////////////////////
//
// AST code generation methods
//
//////////////////////////////////////////////////////////////////////////////

void assign_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# assign_class::code" << endl;
    this->expr->code(s, class_table, class_name, locals_env);

    auto* symbol_entry = locals_env.lookup(name);
    if (symbol_entry != nullptr)
    {
        s << "# assign_class::code: symbol_entry" << std::endl;
        emit_store(ACC, *symbol_entry, FP, s);
        return;
    }

    auto parameter_offsets = class_table->get_node_from_symbol(class_name)->get_attribute_offset_map();
    auto it = parameter_offsets.find(name);
    if (it != parameter_offsets.end())
    {
        s << "# assign_class::code: parameter_offsets" << std::endl;
        emit_store(ACC, it->second + DEFAULT_OBJFIELDS, SELF, s);
        emit_addiu(A1, SELF, (it->second + DEFAULT_OBJFIELDS) * 4, s);
        emit_gc_assign(s);
        return;
    }
}

void static_dispatch_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# static_dispatch_class::code" << endl;
    emit_push(SELF, s);
    emit_push(FP, s);

    for (int i = actual->first(); actual->more(i); i = actual->next(i))
    {
        actual->nth(i)->code(s, class_table, class_name, locals_env);
        emit_push(ACC, s);
    }

    const int continue_label = CgenClassTable::get_next_label();

    this->expr->code(s, class_table, class_name, locals_env);
    emit_bne(ACC, ZERO, continue_label, s);

    Symbol filename = class_table->get_node_from_symbol(class_name)->get_filename();
    emit_load_string(ACC, stringtable.lookup_string(filename->get_string()), s);
    emit_load_imm(T1, this->get_line_number(), s);
    emit_jal("_dispatch_abort", s);
    emit_label_def(continue_label, s);

    emit_move(SELF, ACC, s);
    auto type = type_name;

    emit_partial_load_address(T1, s);
    emit_disptable_ref(type, s);
    s << endl;

    auto offset = class_table->get_node_from_symbol(type)->get_dispatch_offset_map().at(this->name);

    emit_load(T1, offset, T1, s);
    emit_jalr(T1, s);

    emit_pop(FP, s);
    emit_pop(SELF, s);
}

void dispatch_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# dispatch_class::code" << endl;
    emit_push(SELF, s);
    emit_push(FP, s);

    for (int i = actual->first(); actual->more(i); i = actual->next(i))
    {
        actual->nth(i)->code(s, class_table, class_name, locals_env);
        emit_push(ACC, s);
    }

    const int continue_label = CgenClassTable::get_next_label();
    this->expr->code(s, class_table, class_name, locals_env);
    emit_bne(ACC, ZERO, continue_label, s);
    Symbol filename = class_table->get_node_from_symbol(class_name)->get_filename();
    emit_load_string(ACC, stringtable.lookup_string(filename->get_string()), s);
    emit_load_imm(T1, this->get_line_number(), s);
    emit_jal("_dispatch_abort", s);
    emit_label_def(continue_label, s);

    auto type = this->expr->get_type();
    emit_move(SELF, ACC, s);
    if (type == SELF_TYPE)
    {
        type = class_name;
    }
    emit_load(T1, 2, ACC, s);
    s << " # Load dispatch table address " << this->name->get_string() << endl;

    auto offset = class_table->get_node_from_symbol(type)->get_dispatch_offset_map().at(this->name);

    emit_load(T1, offset, T1, s);
    emit_jalr(T1, s);

    emit_pop(FP, s);
    emit_pop(SELF, s);
}

void cond_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# cond_class::code" << endl;
    const int false_label = CgenClassTable::get_next_label();
    const int end_label = CgenClassTable::get_next_label();

    this->pred->code(s, class_table, class_name, locals_env);
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
    emit_beqz(T1, false_label, s);
    this->then_exp->code(s, class_table, class_name, locals_env);
    emit_jump(end_label, s);
    emit_label_def(false_label, s);
    this->else_exp->code(s, class_table, class_name, locals_env);
    emit_label_def(end_label, s);
}

void loop_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# loop_class::code" << endl;
    const int loop_label = CgenClassTable::get_next_label();
    const int exit_label = CgenClassTable::get_next_label();

    emit_label_def(loop_label, s);
    this->pred->code(s, class_table, class_name, locals_env);
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
    emit_beqz(T1, exit_label, s);

    this->body->code(s, class_table, class_name, locals_env);
    emit_jump(loop_label, s);

    emit_label_def(exit_label, s);
    emit_load_imm(ACC, 0, s);
}

void typcase_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# typcase_class::code" << endl;

    std::vector<std::pair<branch_class*, int>> branch_depths {};
    branch_depths.reserve(cases->len());
    for (int i = cases->first(); cases->more(i); i = cases->next(i))
    {
        auto current_case = dynamic_cast<branch_class*>(cases->nth(i));
        int current_depth {};
        auto current_branch = current_case->get_type_decl();
        CgenNodeP current_class = class_table->get_node_from_symbol(current_branch);
        while (current_class->get_parentnd()->get_name() != No_class)
        {
            current_depth++;
            current_class = current_class->get_parentnd();
        }
        branch_depths.push_back(std::make_pair(current_case, current_depth));
    }
    std::sort(
        branch_depths.begin(),
        branch_depths.end(),
        [](const std::pair<branch_class*, int>& a, const std::pair<branch_class*, int>& b) {
            return a.second > b.second;
        }
    );

    const int non_void_label = CgenClassTable::get_next_label();
    this->expr->code(s, class_table, class_name, locals_env);

    emit_push(ACC, s);
    emit_bne(ACC, ZERO, non_void_label, s);
    Symbol filename = class_table->get_node_from_symbol(class_name)->get_filename();
    emit_load_string(ACC, stringtable.lookup_string(filename->get_string()), s);
    emit_load_imm(T1, this->get_line_number(), s);
    emit_jal("_case_abort2", s);
    emit_label_def(non_void_label, s);
    emit_load(T1, 0, ACC, s);

    const int case_exit_label = CgenClassTable::get_next_label();
    std::set<Symbol> visited_classes {};
    s << "\t# start branches" << std::endl;
    for (const auto& [branch, _] : branch_depths)
    {
        s << "\t# start branch: " << branch->get_name()->get_string() << std::endl;
        const int branch_exit_label = CgenClassTable::get_next_label();
        const int branch_do_label = CgenClassTable::get_next_label();

        std::vector<CgenNodeP> descendants {class_table->get_node_from_symbol(branch->get_type_decl())};
        for (size_t i = 0; i < descendants.size(); i++)
        {
            auto tag = class_table->get_tag_from_symbol(descendants[i]->get_name());
            emit_load_imm(T2, tag, s);
            emit_beq(T1, T2, branch_do_label, s);

            const auto& children = descendants[i]->get_children();
            for (const auto& child : children)
            {
                if (visited_classes.find(child->get_name()) == visited_classes.end())
                {
                    descendants.push_back(child);
                    visited_classes.insert(child->get_name());
                }
            }
        }
        emit_jump(branch_exit_label, s);
        emit_label_def(branch_do_label, s);

        locals_env.enterscope();
        locals_env.add_local_id(branch->get_name());

        visited_classes.insert(branch->get_type_decl());

        branch->get_expr()->code(s, class_table, class_name, locals_env);

        emit_addiu(SP, SP, 4, s);

        locals_env.exitscope();

        emit_jump(case_exit_label, s);

        emit_label_def(branch_exit_label, s);
    }
    emit_jal("_case_abort", s);

    emit_label_def(case_exit_label, s);
}

void block_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# block_class::code" << endl;
    for (int i = body->first(); body->more(i); i = body->next(i))
    {
        body->nth(i)->code(s, class_table, class_name, locals_env);
    }
}

void let_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# let_class::code" << endl;
    locals_env.enterscope();

    if (dynamic_cast<no_expr_class*>(this->init) == nullptr)
    {
        this->init->code(s, class_table, class_name, locals_env);
    }
    else
    {
        if (type_decl == Int)
        {
            emit_load_int(ACC, inttable.lookup_string("0"), s);
        }
        else if (type_decl == Bool)
        {
            emit_load_bool(ACC, falsebool, s);
        }
        else if (type_decl == Str)
        {
            emit_load_string(ACC, stringtable.lookup_string(""), s);
        }
        else
        {
            emit_load_imm(ACC, 0, s);
        }
    }
    locals_env.add_local_id(identifier);

    emit_push(ACC, s);
    this->body->code(s, class_table, class_name, locals_env);

    emit_addiu(SP, SP, 4, s);
    locals_env.exitscope();
}

void plus_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# plus_class::code" << endl;
    this->e1->code(s, class_table, class_name, locals_env);
    emit_push(ACC, s);

    this->e2->code(s, class_table, class_name, locals_env);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);
    emit_pop(ACC, s);
    emit_jal_raw(s);
    emit_method_ref(Object, ::copy, s);
    s << endl;
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);

    emit_add(T1, T1, T2, s);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void sub_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# sub_class::code" << endl;
    this->e1->code(s, class_table, class_name, locals_env);
    emit_push(ACC, s);

    this->e2->code(s, class_table, class_name, locals_env);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);
    emit_pop(ACC, s);
    emit_jal_raw(s);
    emit_method_ref(Object, ::copy, s);
    s << endl;
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);

    emit_sub(T1, T1, T2, s);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void mul_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# mul_class::code" << endl;
    this->e1->code(s, class_table, class_name, locals_env);
    emit_push(ACC, s);

    this->e2->code(s, class_table, class_name, locals_env);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);
    emit_pop(ACC, s);
    emit_jal_raw(s);
    emit_method_ref(Object, ::copy, s);
    s << endl;
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);

    emit_mul(T1, T1, T2, s);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void divide_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# divide_class::code" << endl;
    this->e1->code(s, class_table, class_name, locals_env);
    emit_push(ACC, s);

    this->e2->code(s, class_table, class_name, locals_env);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);
    emit_pop(ACC, s);
    emit_jal_raw(s);
    emit_method_ref(Object, ::copy, s);
    s << endl;
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);

    emit_div(T2, T1, T2, s);

    emit_store(T2, DEFAULT_OBJFIELDS, ACC, s);
}

void neg_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# neg_class::code" << endl;
    this->e1->code(s, class_table, class_name, locals_env);
    emit_jal_raw(s);
    emit_method_ref(Object, ::copy, s);
    s << endl;
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
    emit_neg(T1, T1, s);
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void lt_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# lt_class::code" << endl;
    const int exit_label = CgenClassTable::get_next_label();

    this->e1->code(s, class_table, class_name, locals_env);
    emit_push(ACC, s);

    this->e2->code(s, class_table, class_name, locals_env);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);
    emit_pop(ACC, s);
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);

    emit_load_bool(ACC, truebool, s);
    emit_blt(T1, T2, exit_label, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(exit_label, s);
}

void eq_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# eq_class::code" << endl;

    const int exit_label = CgenClassTable::get_next_label();
    this->e1->code(s, class_table, class_name, locals_env);
    emit_push(ACC, s);
    this->e2->code(s, class_table, class_name, locals_env);
    emit_move(T2, ACC, s);
    emit_pop(ACC, s);
    emit_move(T1, ACC, s);

    emit_load_bool(ACC, truebool, s);
    emit_beq(T1, T2, exit_label, s);
    emit_load_bool(A1, falsebool, s);
    emit_jal("equality_test", s);
    emit_label_def(exit_label, s);
}

void leq_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# leq_class::code" << endl;
    const int exit_label = CgenClassTable::get_next_label();
    this->e1->code(s, class_table, class_name, locals_env);
    emit_push(ACC, s);
    this->e2->code(s, class_table, class_name, locals_env);
    emit_load(T2, DEFAULT_OBJFIELDS, ACC, s);
    emit_pop(ACC, s);
    emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);

    emit_load_bool(ACC, truebool, s);
    emit_bleq(T1, T2, exit_label, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(exit_label, s);
}

void comp_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# comp_class::code" << endl;
    const int exit_label = CgenClassTable::get_next_label();

    this->e1->code(s, class_table, class_name, locals_env);

    emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
    emit_load_bool(ACC, truebool, s);
    emit_beqz(T1, exit_label, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(exit_label, s);
}

void int_const_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# int_const_class::code" << endl;
    emit_load_int(ACC, inttable.lookup_string(token->get_string()), s);
}

void string_const_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# string_const_class::code" << endl;
    emit_load_string(ACC, stringtable.lookup_string(token->get_string()), s);
}

void bool_const_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# bool_const_class::code" << endl;
    emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# new__class::code" << endl;
    emit_push(FP, s);
    emit_push(SELF, s);
    if (type_name == SELF_TYPE)
    {
        emit_load(T2, 0, SELF, s);
        emit_load_address(T1, CLASSOBJTAB, s);
        emit_sll(T2, T2, 3, s);
        emit_addu(T1, T1, T2, s);
        emit_load(ACC, 0, T1, s);
        emit_push(T1, s);
        emit_jal_raw(s);
        emit_method_ref(Object, ::copy, s);
        s << endl;
        emit_pop(T1, s);
        emit_move(SELF, ACC, s);
        emit_addiu(T1, T1, 4, s);
        emit_load(T1, 0, T1, s);
        emit_jalr(T1, s);
    }
    else
    {
        emit_partial_load_address(ACC, s);
        emit_protobj_ref(type_name, s);
        s << endl;
        emit_jal_raw(s);
        emit_method_ref(Object, ::copy, s);
        s << endl;
        emit_move(SELF, ACC, s);
        emit_jal_raw(s);
        emit_init_ref(type_name, s);
        s << endl;
    }
    emit_pop(SELF, s);
    emit_pop(FP, s);
}

void isvoid_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# isvoid_class::code" << endl;
    const int exit_label = CgenClassTable::get_next_label();

    this->e1->code(s, class_table, class_name, locals_env);
    emit_move(T1, ACC, s);
    emit_load_bool(ACC, truebool, s);
    emit_beqz(T1, exit_label, s);
    emit_load_bool(ACC, falsebool, s);
    emit_label_def(exit_label, s);
}

void no_expr_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# no_expr_class::code" << endl;
}

void object_class::code(ostream& s, CgenClassTableP class_table, Symbol class_name, Tracker& locals_env)
{
    s << "# object_class::code" << endl;

    s << "#object_class::code: " << name->get_string() << std::endl;
    if (name == self)
    {
        emit_move(ACC, SELF, s);
        s << "#object_class::code: SELF" << std::endl;
        return;
    }

    auto* symbol_entry = locals_env.lookup(name);
    if (symbol_entry != nullptr)
    {
        s << "# object_class::code: symbol_entry" << std::endl;
        emit_load(ACC, *symbol_entry, FP, s);
        return;
    }

    auto parameter_offsets = class_table->get_node_from_symbol(class_name)->get_attribute_offset_map();
    auto it = parameter_offsets.find(name);
    if (it != parameter_offsets.end())
    {
        s << "# object_class::code: parameter_offsets" << std::endl;
        emit_load(ACC, it->second + DEFAULT_OBJFIELDS, SELF, s);
        return;
    }
}
