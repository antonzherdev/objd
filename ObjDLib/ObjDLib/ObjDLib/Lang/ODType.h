//#import "objdcore.h"

@class ODType;
@class ODClassType;
@class ODPType;

@interface ODType : NSObject
+ (id)type;
- (id)init;
- (Class)cls;
- (NSString*)name;
- (NSString*)description;
- (NSUInteger)hash;
- (BOOL)isEqualToOther:(ODType*)other;
@end


@interface ODClassType : ODType
@property (nonatomic, readonly) Class cls;

+ (id)classTypeWithCls:(Class)cls;
- (id)initWithCls:(Class)cls;
- (ODClassType*)type;
- (NSString*)name;
+ (ODClassType*)type;
@end


@interface ODPType : ODType
@property (nonatomic, readonly) Class cls;
@property (nonatomic, readonly) NSString* name;
@property (nonatomic, readonly) NSUInteger size;
@property (nonatomic, readonly) id(^wrap)(void*, NSUInteger);

+ (id)typeWithCls:(Class)cls name:(NSString*)name size:(NSUInteger)size wrap:(id(^)(void*, NSUInteger))wrap;
- (id)initWithCls:(Class)cls name:(NSString*)name size:(NSUInteger)size wrap:(id(^)(void*, NSUInteger))wrap;
- (ODClassType*)type;
+ (ODClassType*)type;
@end


