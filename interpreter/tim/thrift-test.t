// May want more than just numbers. Slow for now, but general
typedef string FLValue
 
struct Notification
{
	// Use a map to take field names to atomic values.
        10: required string notificationType;
        20: required map<string, FLValue> values;
}

struct Query
{
	10: required string relName;
	20: required list<FLValue> arguments;
}

struct QueryReply
{
	10: required set<list<FLValue>> result;
}



service FlowLogInterpreter
{
	void notifyMe(1:Notification notify);
}



service BlackBox
{
	// A black-box answers queries.
	QueryReply doQuery(1:Query q);
	void notifyMe(1:Notification notify);
}

