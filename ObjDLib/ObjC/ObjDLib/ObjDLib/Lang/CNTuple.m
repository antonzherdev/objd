#import "objd.h"
#import "CNTuple.h"

#import "CNType.h"
@implementation CNTuple
static CNClassType* _CNTuple_type;
@synthesize a = _a;
@synthesize b = _b;

+ (instancetype)tupleWithA:(id)a b:(id)b {
    return [[CNTuple alloc] initWithA:a b:b];
}

- (instancetype)initWithA:(id)a b:(id)b {
    self = [super init];
    if(self) {
        _a = a;
        _b = b;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTuple class]) _CNTuple_type = [CNClassType classTypeWithCls:[CNTuple class]];
}

- (NSInteger)compareTo:(CNTuple*)to {
    NSInteger r = [((CNTuple*)(to)).a compareTo:_a];
    if(r == 0) return -[((CNTuple*)(to)).b compareTo:_b];
    else return -r;
}

- (NSString*)description {
    return [NSString stringWithFormat:@"(%@, %@)", _a, _b];
}

+ (CNTuple*)unapplyTuple:(CNTuple*)tuple {
    return tuple;
}

- (CNClassType*)type {
    return [CNTuple type];
}

+ (CNClassType*)type {
    return _CNTuple_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTuple* o = ((CNTuple*)(other));
    return [self.a isEqual:o.a] && [self.b isEqual:o.b];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.a hash];
    hash = hash * 31 + [self.b hash];
    return hash;
}

@end


@implementation CNTuple3
static CNClassType* _CNTuple3_type;
@synthesize a = _a;
@synthesize b = _b;
@synthesize c = _c;

+ (instancetype)tuple3WithA:(id)a b:(id)b c:(id)c {
    return [[CNTuple3 alloc] initWithA:a b:b c:c];
}

- (instancetype)initWithA:(id)a b:(id)b c:(id)c {
    self = [super init];
    if(self) {
        _a = a;
        _b = b;
        _c = c;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTuple3 class]) _CNTuple3_type = [CNClassType classTypeWithCls:[CNTuple3 class]];
}

- (NSInteger)compareTo:(CNTuple3*)to {
    NSInteger r = [((CNTuple3*)(to)).a compareTo:_a];
    if(r == 0) {
        r = [((CNTuple3*)(to)).b compareTo:_b];
        if(r == 0) return -[((CNTuple3*)(to)).c compareTo:_c];
        else return -r;
    } else {
        return -r;
    }
}

- (NSString*)description {
    return [NSString stringWithFormat:@"(%@, %@, %@)", _a, _b, _c];
}

+ (CNTuple3*)unapplyTuple:(CNTuple3*)tuple {
    return tuple;
}

- (CNClassType*)type {
    return [CNTuple3 type];
}

+ (CNClassType*)type {
    return _CNTuple3_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTuple3* o = ((CNTuple3*)(other));
    return [self.a isEqual:o.a] && [self.b isEqual:o.b] && [self.c isEqual:o.c];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.a hash];
    hash = hash * 31 + [self.b hash];
    hash = hash * 31 + [self.c hash];
    return hash;
}

@end


@implementation CNTuple4
static CNClassType* _CNTuple4_type;
@synthesize a = _a;
@synthesize b = _b;
@synthesize c = _c;
@synthesize d = _d;

+ (instancetype)tuple4WithA:(id)a b:(id)b c:(id)c d:(id)d {
    return [[CNTuple4 alloc] initWithA:a b:b c:c d:d];
}

- (instancetype)initWithA:(id)a b:(id)b c:(id)c d:(id)d {
    self = [super init];
    if(self) {
        _a = a;
        _b = b;
        _c = c;
        _d = d;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTuple4 class]) _CNTuple4_type = [CNClassType classTypeWithCls:[CNTuple4 class]];
}

- (NSInteger)compareTo:(CNTuple4*)to {
    NSInteger r = [((CNTuple4*)(to)).a compareTo:_a];
    if(r == 0) {
        r = [((CNTuple4*)(to)).b compareTo:_b];
        if(r == 0) {
            r = [((CNTuple4*)(to)).c compareTo:_c];
            if(r == 0) return -[((CNTuple4*)(to)).d compareTo:_d];
            else return -r;
        } else {
            return -r;
        }
    } else {
        return -r;
    }
}

- (NSString*)description {
    return [NSString stringWithFormat:@"(%@, %@, %@, %@)", _a, _b, _c, _d];
}

+ (CNTuple4*)unapplyTuple:(CNTuple4*)tuple {
    return tuple;
}

- (CNClassType*)type {
    return [CNTuple4 type];
}

+ (CNClassType*)type {
    return _CNTuple4_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTuple4* o = ((CNTuple4*)(other));
    return [self.a isEqual:o.a] && [self.b isEqual:o.b] && [self.c isEqual:o.c] && [self.d isEqual:o.d];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.a hash];
    hash = hash * 31 + [self.b hash];
    hash = hash * 31 + [self.c hash];
    hash = hash * 31 + [self.d hash];
    return hash;
}

@end


@implementation CNTuple5
static CNClassType* _CNTuple5_type;
@synthesize a = _a;
@synthesize b = _b;
@synthesize c = _c;
@synthesize d = _d;
@synthesize e = _e;

+ (instancetype)tuple5WithA:(id)a b:(id)b c:(id)c d:(id)d e:(id)e {
    return [[CNTuple5 alloc] initWithA:a b:b c:c d:d e:e];
}

- (instancetype)initWithA:(id)a b:(id)b c:(id)c d:(id)d e:(id)e {
    self = [super init];
    if(self) {
        _a = a;
        _b = b;
        _c = c;
        _d = d;
        _e = e;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTuple5 class]) _CNTuple5_type = [CNClassType classTypeWithCls:[CNTuple5 class]];
}

- (NSInteger)compareTo:(CNTuple5*)to {
    NSInteger r = [((CNTuple5*)(to)).a compareTo:_a];
    if(r == 0) {
        r = [((CNTuple5*)(to)).b compareTo:_b];
        if(r == 0) {
            r = [((CNTuple5*)(to)).c compareTo:_c];
            if(r == 0) {
                r = [((CNTuple5*)(to)).d compareTo:_d];
                if(r == 0) return -[((CNTuple5*)(to)).e compareTo:_e];
                else return -r;
            } else {
                return -r;
            }
        } else {
            return -r;
        }
    } else {
        return -r;
    }
}

- (NSString*)description {
    return [NSString stringWithFormat:@"(%@, %@, %@, %@, %@)", _a, _b, _c, _d, _e];
}

+ (CNTuple5*)unapplyTuple:(CNTuple5*)tuple {
    return tuple;
}

- (CNClassType*)type {
    return [CNTuple5 type];
}

+ (CNClassType*)type {
    return _CNTuple5_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNTuple5* o = ((CNTuple5*)(other));
    return [self.a isEqual:o.a] && [self.b isEqual:o.b] && [self.c isEqual:o.c] && [self.d isEqual:o.d] && [self.e isEqual:o.e];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.a hash];
    hash = hash * 31 + [self.b hash];
    hash = hash * 31 + [self.c hash];
    hash = hash * 31 + [self.d hash];
    hash = hash * 31 + [self.e hash];
    return hash;
}

@end


