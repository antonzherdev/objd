//#import "objdcore.h"

@class CNType;
@class CNClassType;
@class CNPType;

@interface CNType : NSObject
+ (id)type;
- (id)init;
- (Class)cls;
- (NSString*)name;
- (NSString*)description;
- (NSUInteger)hash;
- (BOOL)isEqualToOther:(CNType *)other;
@end


@interface CNClassType : CNType
@property (nonatomic, readonly) Class cls;

+ (id)classTypeWithCls:(Class)cls;
- (id)initWithCls:(Class)cls;
- (CNClassType *)type;
- (NSString*)name;
+ (CNClassType *)type;
@end


@interface CNPType : CNType
@property (nonatomic, readonly) Class cls;
@property (nonatomic, readonly) NSString* name;
@property (nonatomic, readonly) NSUInteger size;
@property (nonatomic, readonly) id(^wrap)(void*, NSUInteger);

+ (id)typeWithCls:(Class)cls name:(NSString*)name size:(NSUInteger)size wrap:(id(^)(void*, NSUInteger))wrap;
- (id)initWithCls:(Class)cls name:(NSString*)name size:(NSUInteger)size wrap:(id(^)(void*, NSUInteger))wrap;
- (CNClassType *)type;
+ (CNClassType *)type;
@end


