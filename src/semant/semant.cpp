

#include "semant.hpp"
#include "utilities.hpp"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include <algorithm>
#include <format>
#include <map>
#include <set>

extern int semant_debug;
extern char* curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol arg, arg2, Bool, concat, cool_abort, copy, Int, in_int, in_string, IO, length, Main, main_meth, No_class,
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

//////////////////////////////////////////////////////////////////////
//
// ClassTable implementation
//
//////////////////////////////////////////////////////////////////////

ClassTable::ClassTable(Classes classes, std::ostream& error_stream)
    : semant_errors(0)
    , error_stream(error_stream)
{
    install_basic_classes();
    auto i = classes->first();
    while (classes->more(i))
    {
        add_class(classes->nth(i));
        i = classes->next(i);
    }
}

void ClassTable::add_class(Class_ c)
{
    if (class_name_to_class_map.contains(c->get_name()))
    {
        semant_error(c) << "Class " << c->get_name() << " already defined." << std::endl;
        return;
    }
    class_name_to_class_map[c->get_name()] = c;
    if (c->get_parent() != No_class)
    {
        auto parent = c->get_parent();
        class_to_parent_map[c->get_name()] = parent;
        if (class_to_children_map.find(parent) == class_to_children_map.end())
        {
            class_to_children_map[parent] = {};
        }
        class_to_children_map[parent].push_back(c->get_name());
    }
}

void ClassTable::validate_basic_structure()
{
    // Do these checks individually for clarity.

    // Main should exist.
    if (class_name_to_class_map.find(Main) == class_name_to_class_map.end())
    {
        semant_error() << "Class Main is not defined." << std::endl;
    }

    // Reserved class names should not exist.
    std::vector<Symbol> reserved_class_names {No_class, SELF_TYPE, No_type};
    for (const auto& c : reserved_class_names)
    {
        auto reserved_class_it = class_name_to_class_map.find(c);
        if (reserved_class_it != class_name_to_class_map.end())
        {
            semant_error((*reserved_class_it).second) << c << " class name is reserved." << std::endl;
        }
    }

    // Main or basic classes cannot have children.
    std::vector<Symbol> no_children_classes {Main, Bool, Int, Str};
    for (const auto& c : no_children_classes)
    {
        auto class_children_it = class_to_children_map.find(c);
        if (class_children_it != class_to_children_map.end())
        {
            auto class_children = (*class_children_it).second;
            if (class_children.size() > 0)
            {
                for (auto child : class_children)
                {
                    semant_error(class_name_to_class_map[child])
                        << child << " cannot inherit from " << c << "." << std::endl;
                }
            }
        }
    }

    // Cannot inherit itself.
    // This can be caught as a cycle, but do the check here for readability.
    for (auto [class_symbol, parent_class_symbol] : class_to_parent_map)
    {
        if (class_symbol == parent_class_symbol)
        {
            semant_error(class_name_to_class_map[class_symbol]) << " class cannot inherit from itself." << std::endl;
        }
    }

    // Parent should be a class.
    for (auto [class_symbol, parent_class_symbol] : class_to_parent_map)
    {
        if (class_name_to_class_map.find(parent_class_symbol) == class_name_to_class_map.end())
        {
            semant_error(class_name_to_class_map[class_symbol]) << "Symbol " << parent_class_symbol << " not found in class table." << std::endl;
        }
    }

    check_cycles();
}

void ClassTable::check_cycles()
{
    // Check for cycles in the inheritance graph
    // It's easier to do via DFS using the parents.
    // There can only be one parent per class.

    std::set<Symbol> visited {};
    std::set<Symbol> visiting {};

    for (auto [current_class_symbol, parent_class_symbol] : class_to_parent_map)
    {
        if (visited.contains(current_class_symbol))
        {
            continue;
        }
        visiting.insert(current_class_symbol);

        while (parent_class_symbol != No_class)
        {
            if (visited.contains(parent_class_symbol))
            {
                break;
            }
            if (visiting.contains(parent_class_symbol))
            {
                semant_error(class_name_to_class_map[current_class_symbol])
                    << "Cycle detected at: " << parent_class_symbol << "." << std::endl;
                break;
            }
            visiting.insert(parent_class_symbol);

            parent_class_symbol = No_class;
            auto parent_class_it = class_to_parent_map.find(parent_class_symbol);
            if (parent_class_it != class_to_parent_map.end())
            {
                parent_class_symbol = (*parent_class_it).second;
            }
        }
        for (const auto& s : visiting)
        {
            visited.insert(s);
        }
        visiting.clear();
    }
}

void ClassTable::install_basic_classes()
{

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class = class_(
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
    );

    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = class_(
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
    );

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class = class_(Int, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class = class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class = class_(
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
    );

    add_class(Object_class);
    add_class(IO_class);
    add_class(Int_class);
    add_class(Bool_class);
    add_class(Str_class);
}

void ClassTable::create_class_to_methods_map()
{
    // For each class, add all methods defined there.
    for (auto [class_symbol, class_] : class_name_to_class_map)
    {
        auto features = class_->get_features();
        auto i = features->first();
        class_to_method_names_map[class_symbol] = std::set<Symbol> {};
        while (features->more(i))
        {
            auto feature = features->nth(i);
            auto method_ptr = dynamic_cast<method_class*>(feature);
            if (method_ptr != nullptr)
            {
                // Need to make sure no 2 methods have the same name.
                if (class_to_method_names_map[class_symbol].find(method_ptr->get_name()) !=
                    class_to_method_names_map[class_symbol].end())
                {
                    semant_error(class_name_to_class_map[class_symbol])
                        << "Method " << method_ptr->get_name() << " is already defined." << std::endl;
                }

                class_to_method_names_map[class_symbol].insert(method_ptr->get_name());
                class_method_pair_to_method_obj_map.insert(
                    {{class_symbol, method_ptr->get_name()}, *method_ptr}
                );
            }
            i = features->next(i);
        }
    }

    for (auto [class_symbol, class_method_names] : class_to_method_names_map)
    {
        for (auto method_name : class_method_names)
        {
            auto method_obj = class_method_pair_to_method_obj_map.at({class_symbol, method_name});

            // Return type must be defined.
            auto return_type = method_obj.get_return_type();
            if (return_type != SELF_TYPE && !class_name_to_class_map.contains(return_type))
            {
                semant_error(class_name_to_class_map[class_symbol])
                    << "Return type " << return_type << " does not exist in class table." << std::endl;
            }

            auto formals = method_obj.get_formals();
            std::set<Symbol> formal_names {};

            auto j = formals->first();
            while (formals->more(j))
            {
                const auto& formal = formals->nth(j);
                const auto& formal_name = formal->get_name();

                // Formal name should not be in class table.
                if (class_name_to_class_map.contains(formal_name))
                {
                    semant_error(class_name_to_class_map[class_symbol])
                        << "Formal name " << formal_name << " is already defined in class table." << std::endl;
                }

                // Formal name must not be self.
                if (formal_name == self)
                {
                    semant_error(class_name_to_class_map[class_symbol])
                        << "self is not allowed as an argument name." << std::endl;
                }
                // Formal name must not be duplicate.
                else if (formal_names.contains(formal_name))
                {
                    semant_error(class_name_to_class_map[class_symbol])
                        << "Duplicate formal name: " << formal_name << std::endl;
                }
                formal_names.insert(formal_name);

                // Formal type must be defined.
                const auto& formal_type = formal->get_type_decl();
                if (class_name_to_class_map.find(formal_type) == class_name_to_class_map.end())
                {
                    semant_error(class_name_to_class_map[class_symbol])
                        << "Formal type " << formal_type << " does not exist in class table." << std::endl;
                }
                j = formals->next(j);
            }
        }
    }

    // Now need to verify override rules are followed.
    // That means overriding does not change the signature.
    for (auto [class_symbol, class_method_names] : class_to_method_names_map)
    {
        auto parent_class_symbol = class_to_parent_map[class_symbol];
        while (parent_class_symbol != No_class && parent_class_symbol != Symbol())
        {
            auto parent_class_method_names = class_to_method_names_map[parent_class_symbol];
            std::vector<Symbol> common_method_names {};
            std::set_intersection(
                class_method_names.begin(),
                class_method_names.end(),
                parent_class_method_names.begin(),
                parent_class_method_names.end(),
                std::back_inserter(common_method_names)
            );
            for (auto method_name : common_method_names)
            {
                auto method_obj = class_method_pair_to_method_obj_map.at({class_symbol, method_name});
                if (!method_obj.can_override(
                        class_method_pair_to_method_obj_map.at({parent_class_symbol, method_name})
                    ))
                {
                    semant_error(class_name_to_class_map[class_symbol])
                        << "Method " << method_name << " overrides with a different signature." << std::endl;
                }
            }
            parent_class_symbol = class_to_parent_map[parent_class_symbol];
        }
    }
}

void ClassTable::verify_attributes()
{
    std::set<Symbol> default_classes {Object, IO, Int, Bool, Str};
    // For each class, verify the types of the attributes.

    std::map<Symbol, std::set<Symbol>> class_to_attr_names_map {};

    for (auto [class_symbol, class_] : class_name_to_class_map)
    {
        auto features = class_->get_features();
        auto i = features->first();

        // Inheritance, verify attributes are not overridden.

        class_to_attr_names_map[class_symbol] = std::set<Symbol> {};
        while (features->more(i))
        {
            auto feature = features->nth(i);
            auto attr_ptr = dynamic_cast<attr_class*>(feature);
            if (attr_ptr != nullptr)
            {
                auto attr_name = attr_ptr->get_name();
                auto attr_type = attr_ptr->get_type();

                // Self is not allowed as an attribute name.
                if (attr_name == self)
                {
                    semant_error(class_name_to_class_map[class_symbol])
                        << "self is not allowed as an attribute name." << std::endl;
                }
                // Need to verify no 2 attributes have the same name.
                if (class_to_attr_names_map[class_symbol].find(attr_name) !=
                    class_to_attr_names_map[class_symbol].end())
                {
                    semant_error(class_name_to_class_map[class_symbol])
                        << "Attribute " << attr_name << " is already defined." << std::endl;
                }
                class_to_attr_names_map[class_symbol].insert(attr_name);

                if (attr_type != prim_slot && !class_name_to_class_map.contains(attr_type))
                {
                    semant_error(class_name_to_class_map[class_symbol])
                        << "Attribute type " << attr_type << " does not exist in class table." << std::endl;
                }
            }
            i = features->next(i);
        }
    }

    // At this point, each class has a correct set of attributes, if no error has
    // been raised above. Ideally we would be constructing a map from class name
    // to all attributes in the class, including inherited attributes. We would
    // then keep that for later type checking. However, for now, we will later be
    // using other data structures to conform to the guidelines for this project.

    for (auto [class_symbol, class_attr_names] : class_to_attr_names_map)
    {
        auto parent_class_symbol = class_to_parent_map[class_symbol];
        while (parent_class_symbol != No_class && parent_class_symbol != Symbol())
        {
            std::set<Symbol> collided_attributes {};
            std::set_intersection(
                class_attr_names.begin(),
                class_attr_names.end(),
                class_to_attr_names_map[parent_class_symbol].begin(),
                class_to_attr_names_map[parent_class_symbol].end(),
                std::inserter(collided_attributes, collided_attributes.begin())
            );
            if (collided_attributes.size() > 0)
            {
                for (auto attr_name : collided_attributes)
                {
                    semant_error(class_name_to_class_map[class_symbol])
                        << "Attribute " << attr_name << " in class " << class_symbol
                        << " is already defined in parrent class " << parent_class_symbol << endl;
                }
            }
            parent_class_symbol = class_to_parent_map[parent_class_symbol];
        }
    }
}

std::stack<Symbol> ClassTable::get_inheritance_stack(Symbol class_name) const
{
    std::stack<Symbol> inheritance_stack {};
    inheritance_stack.push(class_name);

    auto parent_class_symbol = class_to_parent_map.at(class_name);
    while (parent_class_symbol != No_class && parent_class_symbol != Symbol())
    {
        inheritance_stack.push(parent_class_symbol);
        parent_class_symbol = class_to_parent_map.at(parent_class_symbol);
    }
    return inheritance_stack;
}

void ClassTable::init_object_environment(Symbol class_name, SymbolTable<Symbol, Symbol>& object_env) const
{
    auto inheritance_stack = get_inheritance_stack(class_name);
    while (!inheritance_stack.empty())
    {
        object_env.enterscope();

        auto class_symbol = inheritance_stack.top();
        inheritance_stack.pop();

        auto class_ = class_name_to_class_map.at(class_symbol);
        auto features = class_->get_features();
        auto i = features->first();
        while (features->more(i))
        {
            auto feature = features->nth(i);
            auto attr_ptr = dynamic_cast<attr_class*>(feature);
            if (attr_ptr != nullptr)
            {
                object_env.addid(attr_ptr->get_name(), new Symbol(attr_ptr->get_type()));
            }
            i = features->next(i);
        }
    }
    object_env.addid(self, new Symbol(SELF_TYPE));
}

void ClassTable::populate_expressions_types() const
{
    // For each class, populate the types of the expressions.
    std::set<Symbol> default_classes {Object, IO, Int, Bool, Str};

    for (auto [class_symbol, class_] : class_name_to_class_map)
    {
        if (default_classes.contains(class_symbol))
        {
            continue;
        }

        SymbolTable<Symbol, Symbol> object_environment {};
        init_object_environment(class_symbol, object_environment);

        auto features = class_->get_features();
        auto i = features->first();
        try
        {
            while (features->more(i))
            {
                auto feature = features->nth(i);
                feature->typecheck(object_environment, *this, class_symbol);
                i = features->next(i);
            }
        }
        catch (const std::exception& e)
        {
            error_stream << e.what() << std::endl;
        }
    }
}

bool ClassTable::conforms(Symbol left_class, Symbol right_class, Symbol current_class) const
{
    // left_class <= right_class

    if (left_class == SELF_TYPE && right_class == SELF_TYPE)
    {
        return true;
    }

    if (left_class != SELF_TYPE && right_class == SELF_TYPE)
    {
        return false;
    }

    if (left_class == SELF_TYPE)
    {
        left_class = current_class;
    }

    auto current_class_symbol = left_class;
    while (current_class_symbol != No_class && current_class_symbol != Symbol())
    {
        if (current_class_symbol == right_class)
        {
            return true;
        }
        current_class_symbol = class_to_parent_map.at(current_class_symbol);
    }
    return false;
}

Symbol ClassTable::lub(Symbol left_class, Symbol right_class, Symbol current_class) const
{
    if (left_class == No_type)
    {
        return right_class;
    }

    if (right_class == No_type)
    {
        return left_class;
    }

    if (left_class == SELF_TYPE)
    {
        left_class = current_class;
    }

    if (right_class == SELF_TYPE)
    {
        right_class = current_class;
    }

    auto left_inheritance_stack = get_inheritance_stack(left_class);
    auto right_inheritance_stack = get_inheritance_stack(right_class);

    auto lub = Object;

    while (!left_inheritance_stack.empty() && !right_inheritance_stack.empty())
    {
        if (left_inheritance_stack.top() != right_inheritance_stack.top())
        {
            return lub;
        }
        lub = left_inheritance_stack.top();
        left_inheritance_stack.pop();
        right_inheritance_stack.pop();
    }

    return lub;
}

bool ClassTable::is_valid_class(Symbol class_name) const
{
    return class_name_to_class_map.contains(class_name);
}

Class_ ClassTable::get_class(Symbol class_name) const
{
    return class_name_to_class_map.at(class_name);
}

bool ClassTable::has_method(Symbol class_name, Symbol method_name) const
{
    auto current_class = class_name;
    while (current_class != No_class && current_class != Symbol())
    {
        if (class_to_method_names_map.at(current_class).find(method_name) !=
            class_to_method_names_map.at(current_class).end())
        {
            return true;
        }
        current_class = class_to_parent_map.at(current_class);
    }
    return false;
}

method_class ClassTable::get_method(Symbol class_name, Symbol method_name) const
{
    auto current_class = class_name;
    while (current_class != No_class && current_class != Symbol())
    {
        if (class_to_method_names_map.at(current_class).find(method_name) !=
            class_to_method_names_map.at(current_class).end())
        {
            return class_method_pair_to_method_obj_map.at({current_class, method_name});
        }
        current_class = class_to_parent_map.at(current_class);
    }
    throw std::runtime_error(std::string("Method not found in class table."));
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c) const
{
    return semant_error(c->get_filename(), c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node* t) const
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error() const
{
    semant_errors++;
    return error_stream;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.hpp')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */

void program_class::semant(std::ostream& error_stream)
{
    initialize_constants();
    /* ClassTable constructor may do some semantic analysis */
    ClassTable* classtable = new ClassTable(classes, error_stream);
    classtable->validate_basic_structure();
    if (classtable->errors())
    {
        error_stream << "Compilation halted due to static semantic errors." << std::endl;
        return;
    }

    classtable->create_class_to_methods_map();
    classtable->verify_attributes();
    if (classtable->errors())
    {
        error_stream << "Compilation halted due to static semantic errors." << std::endl;
        return;
    }

    classtable->populate_expressions_types();

    if (classtable->errors())
    {
        error_stream << "Compilation halted due to static semantic errors." << std::endl;
        return;
    }
}

//////////////////////////////////////////////////////////////////////////////
//
// AST typecheck methods for semantic analysis
//
//////////////////////////////////////////////////////////////////////////////

void attr_class::typecheck(SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class)
{
    init->typecheck(object_env, classtable, current_class);
}

void method_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    object_env.enterscope();
    auto i = formals->first();
    while (formals->more(i))
    {
        object_env.addid(formals->nth(i)->get_name(), new Symbol(formals->nth(i)->get_type_decl()));
        i = formals->next(i);
    }
    auto actual_return_type = expr->typecheck(object_env, classtable, current_class);
    object_env.exitscope();

    if (!classtable.conforms(actual_return_type, return_type, current_class))
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Method " << name << " returns type " << actual_return_type
            << " which does not conform to the return type " << return_type << "." << std::endl;
    }
}

Symbol assign_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    auto rtype = expr->typecheck(object_env, classtable, current_class);

    set_type(rtype);

    auto ltype_ptr = object_env.lookup(name);
    Symbol ltype = Object;

    if (name == self)
    {
        classtable.semant_error(classtable.get_class(current_class)) << "Cannot assign to self." << std::endl;
    }
    else if (!ltype_ptr)
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Symbol " << name << " not found in class table." << std::endl;
        ltype = rtype;
    }
    else if (!classtable.conforms(rtype, *ltype_ptr, current_class))
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Method " << name << " returns type " << rtype << " which is not a subclass of " << *ltype_ptr << "."
            << std::endl;
        ltype = rtype;
    }
    else
    {
        ltype = *ltype_ptr;
    }
    set_type(ltype);
    return ltype;
}

Symbol static_dispatch_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    auto expr_type = expr->typecheck(object_env, classtable, current_class);

    auto result_type = Object;

    if (type_name == SELF_TYPE)
    {
        classtable.semant_error(classtable.get_class(current_class)) << "Cannot use SELF_TYPE as a type name." << std::endl;
    }
    else if (!classtable.is_valid_class(type_name))
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Symbol " << type_name << " not found in class table." << std::endl;
    }
    else if (!classtable.conforms(expr_type, type_name, current_class))
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Expression type " << expr_type << " does not conform to the type " << type_name << "." << std::endl;
    }
    else if (!classtable.has_method(type_name, name))
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Method " << name << " not found in class table." << std::endl;
    }
    else
    {
        auto method = classtable.get_method(type_name, name);
        auto formals = method.get_formals();

        bool has_correct_argument_len = actual->len() == formals->len();
        if (!has_correct_argument_len)
        {
            classtable.semant_error(classtable.get_class(current_class))
                << "Method " << name << " has " << actual->len() << " arguments, but " << formals->len()
                << " are expected." << std::endl;
        }

        auto i = actual->first();
        while (actual->more(i))
        {
            auto actual_type = actual->nth(i);
            auto current_actual_type = actual->nth(i)->typecheck(object_env, classtable, current_class);

            if (has_correct_argument_len)
            {
                if (!classtable.conforms(current_actual_type, formals->nth(i)->get_type_decl(), current_class))
                {
                    classtable.semant_error(classtable.get_class(current_class))
                        << "Argument " << i << " has type " << actual_type << " which is not a subclass of "
                        << formals->nth(i)->get_type_decl() << "." << std::endl;
                }
            }

            i = actual->next(i);
        }

        result_type = method.get_return_type();
        if (result_type == SELF_TYPE)
        {
            if (expr_type != SELF_TYPE)
            {
                result_type = expr_type;
            }
            else
            {
                result_type = SELF_TYPE;
            }
        }
    }

    set_type(result_type);
    return result_type;
}

Symbol dispatch_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    auto expr_type = expr->typecheck(object_env, classtable, current_class);

    auto result_type = Object;

    auto sanitized_expr_type = expr_type == SELF_TYPE ? current_class : expr_type;

    if (!classtable.has_method(sanitized_expr_type, name))
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Method " << name << " not found in class table." << std::endl;
    }
    else
    {
        auto method = classtable.get_method(sanitized_expr_type, name);
        auto formals = method.get_formals();

        bool has_correct_argument_len = actual->len() == formals->len();
        if (!has_correct_argument_len)
        {
            classtable.semant_error(classtable.get_class(current_class))
                << "Method " << name << " has " << actual->len() << " arguments, but " << formals->len()
                << " are expected." << std::endl;
        }

        auto i = actual->first();
        while (actual->more(i))
        {
            auto actual_type = actual->nth(i);
            auto current_actual_type = actual->nth(i)->typecheck(object_env, classtable, current_class);

            if (has_correct_argument_len)
            {
                if (!classtable.conforms(current_actual_type, formals->nth(i)->get_type_decl(), current_class))
                {
                    classtable.semant_error(classtable.get_class(current_class))
                        << "In call of method " << name << ", type " << current_actual_type
                        << " of parameter " << formals->nth(i)->get_name()
                        << " does not conform to declared type " << formals->nth(i)->get_type_decl() << "."
                        << std::endl;
                }
            }

            i = actual->next(i);
        }

        result_type = method.get_return_type();
        if (result_type == SELF_TYPE)
        {
            if (expr_type != SELF_TYPE)
            {
                result_type = expr_type;
            }
            else
            {
                result_type = SELF_TYPE;
            }
        }
    }

    set_type(result_type);
    return result_type;
}

Symbol cond_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    auto pred_type = pred->typecheck(object_env, classtable, current_class);
    auto then_type = then_exp->typecheck(object_env, classtable, current_class);
    auto else_type = else_exp->typecheck(object_env, classtable, current_class);

    auto result_type = Object;

    if (pred_type != Bool)
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Cond operation requires a Bool predicate. but got " << pred_type << "." << std::endl;
    }
    else
    {
        result_type = classtable.lub(then_type, else_type, current_class);
    }
    set_type(result_type);
    return result_type;
}

Symbol loop_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    auto pred_type = pred->typecheck(object_env, classtable, current_class);
    auto body_type = body->typecheck(object_env, classtable, current_class);

    auto result_type = Object;

    if (pred_type != Bool)
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Loop operation requires a Bool predicate." << std::endl;
    }

    set_type(result_type);
    return result_type;
}

Symbol typcase_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    // First check that the branches have unique types:
    std::set<Symbol> branch_types {};
    auto i = cases->first();
    while (cases->more(i))
    {
        if (branch_types.contains(cases->nth(i)->get_type_decl()))
        {
            classtable.semant_error(classtable.get_class(current_class))
                << "Case " << cases->nth(i)->get_name() << " has the same type as another case." << std::endl;
            set_type(Object);
            return Object;
        }
        branch_types.insert(cases->nth(i)->get_type_decl());
        i = cases->next(i);
    }

    auto expr_type = expr->typecheck(object_env, classtable, current_class);
    auto result_type = Object;

    i = cases->first();
    auto lub = No_type;
    while (cases->more(i))
    {
        object_env.enterscope();
        object_env.addid(cases->nth(i)->get_name(), new Symbol(cases->nth(i)->get_type_decl()));
        auto branch_type = cases->nth(i)->typecheck(object_env, classtable, current_class);
        lub = classtable.lub(lub, branch_type, current_class);
        object_env.exitscope();
        i = cases->next(i);
    }

    if (!classtable.conforms(lub, expr_type == SELF_TYPE ? current_class : expr_type, current_class))
    {
        classtable.semant_error(classtable.get_class(current_class))
            << std::format("Expression type {} is not a subclass of {}.", expr_type->get_string(), lub->get_string())
            << std::endl;
    }
    else
    {
        result_type = lub;
    }

    set_type(result_type);
    return result_type;
}

Symbol branch_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    auto expr_type = expr->typecheck(object_env, classtable, current_class);

    auto result_type = Object;
    if (!classtable.is_valid_class(type_decl))
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Type " << type_decl << " not found in class table." << std::endl;
    }
    else if (!classtable.conforms(expr_type, type_decl, current_class))
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Expression type " << expr_type << " is not a subclass of " << type_decl << "." << std::endl;
    }
    else
    {
        result_type = type_decl;
    }

    return expr_type;
}

Symbol block_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    auto i = body->first();
    auto expr_type = Object;
    while (body->more(i))
    {
        expr_type = body->nth(i)->typecheck(object_env, classtable, current_class);
        i = body->next(i);
    }

    set_type(expr_type);
    return expr_type;
}

Symbol let_class::typecheck(SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class)
{
    auto init_type = init->typecheck(object_env, classtable, current_class);

    auto result_type = Object;

    if (type_decl != SELF_TYPE && !classtable.is_valid_class(type_decl))
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "In let expression, type " << type_decl << " not found in class table." << std::endl;
    }
    else if (type_decl != SELF_TYPE && init_type != No_type &&
             !classtable.conforms(init_type, type_decl, current_class))
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Initialization type " << init_type << " is not a subclass of " << type_decl << "." << std::endl;
    }
    else if (identifier == self)
    {
        classtable.semant_error(classtable.get_class(current_class)) << "Cannot override self." << std::endl;
    }
    else
    {
        object_env.enterscope();
        object_env.addid(identifier, new Symbol(type_decl));
        auto body_result_type = body->typecheck(object_env, classtable, current_class);
        object_env.exitscope();
        result_type = body_result_type;
    }

    set_type(result_type);
    return result_type;
}

Symbol plus_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    auto e1_type = e1->typecheck(object_env, classtable, current_class);
    auto e2_type = e2->typecheck(object_env, classtable, current_class);
    auto type = Int;

    if (e1_type != Int || e2_type != Int)
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Plus operation requires two Int arguments." << std::endl;
        type = Object;
    }

    set_type(type);
    return type;
}

Symbol sub_class::typecheck(SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class)
{
    auto e1_type = e1->typecheck(object_env, classtable, current_class);
    auto e2_type = e2->typecheck(object_env, classtable, current_class);
    auto type = Int;

    if (e1_type != Int || e2_type != Int)
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Sub operation requires two Int arguments." << std::endl;
        type = Object;
    }

    set_type(type);
    return type;
}

Symbol mul_class::typecheck(SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class)
{
    auto e1_type = e1->typecheck(object_env, classtable, current_class);
    auto e2_type = e2->typecheck(object_env, classtable, current_class);
    auto type = Int;

    if (e1_type != Int || e2_type != Int)
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Mul operation requires two Int arguments." << std::endl;
        type = Object;
    }

    set_type(type);
    return type;
}

Symbol divide_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    auto e1_type = e1->typecheck(object_env, classtable, current_class);
    auto e2_type = e2->typecheck(object_env, classtable, current_class);
    auto type = Int;

    if (e1_type != Int || e2_type != Int)
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Divide operation requires two Int arguments." << std::endl;
        type = Object;
    }

    set_type(type);
    return type;
}

Symbol neg_class::typecheck(SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class)
{
    auto e1_type = e1->typecheck(object_env, classtable, current_class);

    auto type = Int;

    if (e1_type != Int)
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Neg operation requires an Int argument." << std::endl;
        type = Object;
    }

    set_type(type);
    return type;
}

Symbol lt_class::typecheck(SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class)
{
    auto e1_type = e1->typecheck(object_env, classtable, current_class);
    auto e2_type = e2->typecheck(object_env, classtable, current_class);
    auto type = Bool;

    if (e1_type != Int || e2_type != Int)
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Lt operation requires two Int arguments." << std::endl;
        type = Object;
    }

    set_type(type);
    return type;
}

Symbol eq_class::typecheck(SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class)
{
    auto e1_type = e1->typecheck(object_env, classtable, current_class);
    auto e2_type = e2->typecheck(object_env, classtable, current_class);
    auto type = Bool;

    std::set<Symbol> restricted_types {Int, Bool, Str};

    if (restricted_types.find(e1_type) != restricted_types.find(e2_type))
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Incompatible equality comparison between basic types "
            << e1_type << " and " << e2_type << "." << std::endl;
        type = Object;
    }

    set_type(type);
    return type;
}

Symbol leq_class::typecheck(SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class)
{
    auto e1_type = e1->typecheck(object_env, classtable, current_class);
    auto e2_type = e2->typecheck(object_env, classtable, current_class);
    auto type = Bool;

    if (e1_type != Int || e2_type != Int)
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Leq operation requires two Int arguments." << std::endl;
        type = Object;
    }

    set_type(type);
    return type;
}

Symbol comp_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    auto rtype = e1->typecheck(object_env, classtable, current_class);

    set_type(Bool);
    return Bool;
}

Symbol int_const_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    auto type = Int;
    set_type(type);
    return type;
}

Symbol bool_const_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    auto type = Bool;
    set_type(type);
    return type;
}

Symbol string_const_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    auto type = Str;
    set_type(type);
    return type;
}

Symbol new__class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    Symbol type {type_name};
    if (type_name != SELF_TYPE && !classtable.is_valid_class(type_name))
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Symbol " << type_name << " not found in class table." << std::endl;
        type = Object;
    }

    set_type(type);
    return type;
}

Symbol isvoid_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    auto rtype = e1->typecheck(object_env, classtable, current_class);

    set_type(Bool);
    return Bool;
}

Symbol no_expr_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    set_type(No_type);
    return No_type;
}

Symbol object_class::typecheck(
    SymbolTable<Symbol, Symbol>& object_env, const ClassTable& classtable, Symbol current_class
)
{
    Symbol* object_type_ptr = object_env.lookup(name);
    Symbol object_type = Object;
    if (!object_type_ptr)
    {
        classtable.semant_error(classtable.get_class(current_class))
            << "Symbol " << name << " not found in class table." << std::endl;
    }
    else
    {
        object_type = *object_type_ptr;
    }

    set_type(object_type);
    return object_type;
}
