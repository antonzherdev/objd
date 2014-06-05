#import "objdcore.h"
#import "CNObject.h"
#import "CNPointer.h"
@class CNPType;
@class CNClassType;
@class CNString;

@class CNBuffer;
@class CNInt4Buffer;
@class CNFloat4Buffer;

@interface CNBuffer : NSObject {
@public
    CNPType* _tp;
    unsigned int _count;
    void* _bytes;
    void* __pointer;
    unsigned int __position;
}
@property (nonatomic, readonly) CNPType* tp;
@property (nonatomic, readonly) unsigned int count;
@property (nonatomic, readonly) void* bytes;

+ (instancetype)bufferWithTp:(CNPType*)tp count:(unsigned int)count;
- (instancetype)initWithTp:(CNPType*)tp count:(unsigned int)count;
- (CNClassType*)type;
- (unsigned int)stride;
- (NSUInteger)length;
- (void)dealloc;
- (void)reset;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNInt4Buffer : CNBuffer
+ (instancetype)int4BufferWithCount:(unsigned int)count;
- (instancetype)initWithCount:(unsigned int)count;
- (CNClassType*)type;
- (int)get;
- (void)setV:(int)v;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNFloat4Buffer : CNBuffer
+ (instancetype)float4BufferWithCount:(unsigned int)count;
- (instancetype)initWithCount:(unsigned int)count;
- (CNClassType*)type;
- (float)get;
- (void)setV:(float)v;
- (NSString*)description;
+ (CNClassType*)type;
@end


