#import "objdcore.h"
#import "CNCollection.h"
@class ODClassType;
@class CNChain;

@class CNMapDefault;
@class CNHashMapBuilder;
@protocol CNMap;
@protocol CNMutableMap;

@protocol CNMap<CNIterable>
- (id)applyKey:(id)key;
- (id)optKey:(id)key;
- (id<CNIterable>)keys;
- (id<CNIterable>)values;
- (BOOL)containsKey:(id)key;
- (BOOL)isValueEqualKey:(id)key value:(id)value;
- (id<CNMap>)addItem:(CNTuple*)item;
@end


@protocol CNMutableMap<CNMap, CNMutableIterable>
- (void)setKey:(id)key value:(id)value;
- (id)removeForKey:(id)key;
- (id)objectForKey:(id)key orUpdateWith:(id(^)())orUpdateWith;
- (id)modifyBy:(id(^)(id))by forKey:(id)forKey;
- (void)appendItem:(CNTuple*)item;
- (void)removeItem:(CNTuple*)item;
@end


@interface CNMapDefault : NSObject<CNMutableIterable>
@property (nonatomic, readonly) id(^defaultFunc)(id);
@property (nonatomic, readonly) id<CNMutableMap> map;

+ (id)mapDefaultWithDefaultFunc:(id(^)(id))defaultFunc map:(id<CNMutableMap>)map;
- (id)initWithDefaultFunc:(id(^)(id))defaultFunc map:(id<CNMutableMap>)map;
- (ODClassType*)type;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id<CNMutableIterator>)mutableIterator;
- (id)applyKey:(id)key;
- (id<CNIterable>)keys;
- (id<CNIterable>)values;
- (BOOL)containsKey:(id)key;
- (void)setKey:(id)key value:(id)value;
- (id)modifyBy:(id(^)(id))by forKey:(id)forKey;
- (void)appendItem:(CNTuple*)item;
- (void)removeItem:(CNTuple*)item;
- (void)clear;
+ (ODClassType*)type;
@end


@interface CNHashMapBuilder : NSObject<CNBuilder>
@property (nonatomic, readonly) NSMutableDictionary* map;

+ (id)hashMapBuilder;
- (id)init;
- (ODClassType*)type;
- (void)appendItem:(CNTuple*)item;
- (NSDictionary*)build;
+ (ODClassType*)type;
@end


