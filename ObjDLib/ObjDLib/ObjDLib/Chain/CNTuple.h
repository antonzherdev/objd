#import "objdcore.h"
#import "ODObject.h"
@class ODClassType;

@class CNTuple;
@class CNTuple3;
@class CNTuple4;

@interface CNTuple : NSObject<ODComparable>
@property (nonatomic, readonly) id a;
@property (nonatomic, readonly) id b;

+ (id)tupleWithA:(id)a b:(id)b;
- (id)initWithA:(id)a b:(id)b;
- (ODClassType*)type;
- (NSInteger)compareTo:(CNTuple*)to;
- (NSString*)description;
+ (id)unapplyTuple:(CNTuple*)tuple;
+ (ODClassType*)type;
@end


@interface CNTuple3 : NSObject<ODComparable>
@property (nonatomic, readonly) id a;
@property (nonatomic, readonly) id b;
@property (nonatomic, readonly) id c;

+ (id)tuple3WithA:(id)a b:(id)b c:(id)c;
- (id)initWithA:(id)a b:(id)b c:(id)c;
- (ODClassType*)type;
- (NSInteger)compareTo:(CNTuple3*)to;
- (NSString*)description;
+ (id)unapplyTuple:(CNTuple3*)tuple;
+ (ODClassType*)type;
@end


@interface CNTuple4 : NSObject<ODComparable>
@property (nonatomic, readonly) id a;
@property (nonatomic, readonly) id b;
@property (nonatomic, readonly) id c;
@property (nonatomic, readonly) id d;

+ (id)tuple4WithA:(id)a b:(id)b c:(id)c d:(id)d;
- (id)initWithA:(id)a b:(id)b c:(id)c d:(id)d;
- (ODClassType*)type;
- (NSInteger)compareTo:(CNTuple4*)to;
- (NSString*)description;
+ (id)unapplyTuple:(CNTuple4*)tuple;
+ (ODClassType*)type;
@end


