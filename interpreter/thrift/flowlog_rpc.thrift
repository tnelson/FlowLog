/* Apache Thrift RPC definition for FlowLog
 *
 * THIS FILE IS A NON-FINAL TEST. DO NOT RELY UPON IT.
 */

////////////////////////////////////////////////////// 
// What kinds of atomic values get passed back and forth?
// Not necessarily just numbers! An FLValue represents
// some FlowLog term, which is a constant or a variable.
////////////////////////////////////////////////////// 

typedef string FLValue

////////////////////////////////////////////////////// 
// A notification (from either a black-box to the
// controller or vice versa) has this form: a string
// describing the type of notification, and a map 
// containing the fields for that type. E.g. an AppleTV
// registration event would contain fields for the mac addresses
// of the requesting machine and the apple tv in question.
//
// ASSUMPTION: values of fields are atomic.
//////////////////////////////////////////////////////

struct Notification
{
	// Use a map to take field names to atomic values.
        // (The Thrift format requires we assign a number to each field.)
        1: required string notificationType;
        2: required map<string, FLValue> values;
}

//////////////////////////////////////////////////////
// A query to a black-box from the controller asks:
// "For what tuples <X_1, ..., X_n> is R(t_1, ..., t_m)
//  true?" (where each t_1 is either a constant or an X_i).
//
// ASSUMPTION: Is this enough to cover all cases? 
// It covers the Boolean case (where all terms are constant)
// since the result will be either a singleton or non-empty.
//////////////////////////////////////////////////////

struct Query
{
	1: required string relName;
	2: required list<FLValue> arguments;
}

//////////////////////////////////////////////////////
// A reply to a query is a set of tuples. (See above.)
//////////////////////////////////////////////////////

struct QueryReply
{
	1: required set<list<FLValue>> result;
	2: optional string exception_code;
	3: optional string exception_message;
}

//////////////////////////////////////////////////////
// A *SERVICE* is like a Java interface: it defines
// functions that implementors of the service provide.
// Here the FlowLog interpreter has the ability to
// receive notifications from external code.
// 
// (Like in structs, Thrift makes us give a unique number
//  ID for each argument to each function.)
//////////////////////////////////////////////////////

service FlowLogInterpreter
{
	void notifyMe(1:Notification notify);
}

//////////////////////////////////////////////////////
// Likewise, a black-box has the power to receive 
// notifications, but can also be queried.
//////////////////////////////////////////////////////

service BlackBox
{
	// A black-box answers queries.
	QueryReply doQuery(1:Query q);
	void notifyMe(1:Notification notify);
}

