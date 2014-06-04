#import "objdcore.h"
#import "CNObject.h"
#import "CNPointer.h"
@class CNPType;
@class CNClassType;
@class CNString;

@class CNBuffer;
@class CNInt4Buffer;
@class CNFloat4Buffer;

@interface CNBuffer : NSObject
- (CNClassType*)type;
+ (CNClassType*)type;
@end


@interface CNInt4Buffer : CNBuffer {
@protected
    CNPType* _tp;
    unsigned int _count;
    int* _bytes;
    int* __pointer;
    unsigned int __position;
}
@property (nonatomic, readonly) CNPType* tp;
@property (nonatomic, readonly) unsigned int count;
@property (nonatomic, readonly) int* bytes;

+ (instancetype)int4BufferWithCount:(unsigned int)count;
- (instancetype)initWithCount:(unsigned int)count;
- (CNClassType*)type;
- (NSString*)description;
- (unsigned int)stride;
- (NSUInteger)length;
- (void)dealloc;
- (int)get;
- (void)setV:(int)v;
- (void)reset;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNFloat4Buffer : CNBuffer {
@protected
    CNPType* _tp;
    unsigned int _count;
    float* _bytes;
    float* __pointer;
    unsigned int __position;
}
@property (nonatomic, readonly) CNPType* tp;
@property (nonatomic, readonly) unsigned int count;
@property (nonatomic, readonly) float* bytes;

+ (instancetype)float4BufferWithCount:(unsigned int)count;
- (instancetype)initWithCount:(unsigned int)count;
- (CNClassType*)type;
- (NSString*)description;
- (unsigned int)stride;
- (NSUInteger)length;
- (void)dealloc;
- (float)get;
- (void)setV:(float)v;
- (void)reset;
- (NSString*)description;
+ (CNClassType*)type;
@end


