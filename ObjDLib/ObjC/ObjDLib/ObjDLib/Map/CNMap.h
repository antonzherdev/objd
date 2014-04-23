#import "objdcore.h"
#import "CNCollection.h"
@class CNDispatchQueue;
@class CNChain;
@class CNMHashMap;
@class ODClassType;
@class CNImHashMap;

@class CNMap_impl;
@class CNImMap_impl;
@class CNMMap_impl;
@class CNImMapDefault;
@class CNMMapDefault;
@class CNHashMapBuilder;
@protocol CNMap;
@protocol CNImMap;
@protocol CNMMap;

@protocol CNMap<CNIterable>
- (id)applyKey:(id)key;
- (id)optKey:(id)key;
- (id)getKey:(id)key orValue:(id)orValue;
- (id<CNIterable>)keys;
- (id<CNIterable>)values;
- (BOOL)containsKey:(id)key;
- (BOOL)isValueEqualKey:(id)key value:(id)value;
@end


@interface CNMap_impl : CNIterable_impl<CNMap>
@end


@protocol CNImMap<CNMap, CNImIterable>
- (id<CNImMap>)addItem:(CNTuple*)item;
- (id<CNMMap>)mCopy;
@end


@interface CNImMap_impl : CNMap_impl<CNImMap>
- (id<CNMMap>)mCopy;
@end


@protocol CNMMap<CNMap, CNMIterable>
- (void)setKey:(id)key value:(id)value;
- (id)removeForKey:(id)key;
- (id)objectForKey:(id)key orUpdateWith:(id(^)())orUpdateWith;
- (id)modifyKey:(id)key by:(id(^)(id))by;
- (id)takeKey:(id)key;
- (void)appendItem:(CNTuple*)item;
- (BOOL)removeItem:(CNTuple*)item;
- (id<CNImMap>)im;
- (id<CNImMap>)imCopy;
- (void)assignImMap:(id<CNImMap>)imMap;
@end


@interface CNMMap_impl : CNMap_impl<CNMMap>
- (void)appendItem:(CNTuple*)item;
- (BOOL)removeItem:(CNTuple*)item;
- (id<CNImMap>)im;
- (id<CNImMap>)imCopy;
@end


@interface CNImMapDefault : CNImIterable_impl {
@protected
    id<CNImMap> _map;
    id(^_defaultFunc)(id);
}
@property (nonatomic, readonly) id<CNImMap> map;
@property (nonatomic, readonly) id(^defaultFunc)(id);

+ (instancetype)imMapDefaultWithMap:(id<CNImMap>)map defaultFunc:(id(^)(id))defaultFunc;
- (instancetype)initWithMap:(id<CNImMap>)map defaultFunc:(id(^)(id))defaultFunc;
- (ODClassType*)type;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id)applyKey:(id)key;
- (id<CNIterable>)keys;
- (id<CNIterable>)values;
- (BOOL)containsKey:(id)key;
- (BOOL)isEqualMap:(id<CNMap>)map;
- (BOOL)isEqualMapDefault:(CNImMapDefault*)mapDefault;
- (NSUInteger)hash;
- (CNMMapDefault*)mCopy;
+ (ODClassType*)type;
@end


@interface CNMMapDefault : CNMIterable_impl {
@protected
    id<CNMMap> _map;
    id(^_defaultFunc)(id);
}
@property (nonatomic, readonly) id<CNMMap> map;
@property (nonatomic, readonly) id(^defaultFunc)(id);

+ (instancetype)mapDefaultWithMap:(id<CNMMap>)map defaultFunc:(id(^)(id))defaultFunc;
- (instancetype)initWithMap:(id<CNMMap>)map defaultFunc:(id(^)(id))defaultFunc;
- (ODClassType*)type;
- (NSUInteger)count;
- (id<CNIterator>)iterator;
- (id<CNMIterator>)mutableIterator;
- (id)applyKey:(id)key;
- (id<CNIterable>)keys;
- (id<CNIterable>)values;
- (BOOL)containsKey:(id)key;
- (void)setKey:(id)key value:(id)value;
- (id)modifyKey:(id)key by:(id(^)(id))by;
- (void)appendItem:(CNTuple*)item;
- (BOOL)removeItem:(CNTuple*)item;
- (void)clear;
- (CNImMapDefault*)im;
- (CNImMapDefault*)imCopy;
+ (ODClassType*)type;
@end


@interface CNHashMapBuilder : CNBuilder_impl {
@protected
    CNMHashMap* _map;
}
+ (instancetype)hashMapBuilder;
- (instancetype)init;
- (ODClassType*)type;
- (void)appendItem:(CNTuple*)item;
- (CNImHashMap*)build;
+ (ODClassType*)type;
@end


