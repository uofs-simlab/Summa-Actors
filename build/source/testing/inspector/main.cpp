#include "caf/all.hpp"
#include "caf/io/all.hpp"

using namespace caf;

struct struct_1;
struct struct_2;
struct struct_3;

// Add the custom types as messages for actors
CAF_BEGIN_TYPE_ID_BLOCK(custom_type, first_custom_type_id)
    // CAF_ADD_TYPE_ID(custom_type, (struct_1))
    CAF_ADD_TYPE_ID(custom_type, (struct_2))
    CAF_ADD_TYPE_ID(custom_type, (struct_3))
CAF_END_TYPE_ID_BLOCK(custom_type)


struct struct_3 {
    int num_1;
    int num_2;
};

struct struct_2 {
    int num_3;
    int num_4;
};

struct struct_1 {
    struct_3 test_struct_1;
    struct_2 test_struct_2;
};

template<class Inspector>
bool inspect(Inspector& inspector, struct_2& struct_2_inspect) {
    return inspector.object(struct_2_inspect).fields(
                inspector.field("num_3", struct_2_inspect.num_3),
                inspector.field("num_4", struct_2_inspect.num_4));
}
template<class Inspector>
bool inspect(Inspector& inspector, struct_3& struct_3_inspect) {
    return inspector.object(struct_3_inspect).fields(
                inspector.field("num_1", struct_3_inspect.num_1),
                inspector.field("num_2", struct_3_inspect.num_2));
}

// Inspector overload is needed to compile
template <class Inspector>
bool insepct(Inspector& inspector, struct_1& struct_1_inspect, 
    struct_2& struct_2_inspect, struct_3& struct_3_inspect) {
    return inspector.object(struct_1_inspect).fields(
                inspector.object(struct_2_inspect).fields(
                    inspector.field("num_3", struct_2_inspect.num_3),
                    inspector.field("num_4", struct_2_inspect.num_4)),
                inspector.object(struct_3_inspect).fields(
                    inspector.field("num_1", struct_3_inspect.num_1),
                    inspector.filed("num_2", struct_3_inspect.num_2)));
} 

void caf_main(actor_system& sys) {
    scoped_actor self{sys};
    aout(self) << "Started Test Actor \n"; 

}

CAF_MAIN(id_block::custom_type)