type LinuxOnly a = a
type WindowsOnly a = a
type WindowsOnlyV = ()
type LinuxOnlyV = ()

data Address = Address
  (LinuxOnly EffectiveNUMAPolicy)
  Status

data Status = Free | Allocated Allocation

data Allocation = Allocation
  NUMANode
  Base Size
  MappingType
  (WindowsOnly AllocatedMemoryProtection)
  (LinuxOnly LinuxMemoryProtection)
  (WindowsOnly (Maybe ControlFlowGuard))
  (WindowsOnly PageBits)
  HugePageFlags
  (LinuxOnly MemoryProtectionKey)
  WriteWatch
  (LinuxOnly (ReadAdvise, VmFlags))
  PageStatus

data NUMANode = Node Int | NUMA_NO_PREFERRED_NODE WindowsOnlyV
type Base = Int
type Size = Int

data MappingType =
  AnonymousPrivate (LinuxOnly PseudoPath) (WindowsOnly AllocType)
  | AnonymousShared
      (WindowsOnly (Name, AllocShared, SEC_NOCACHE, SEC_WRITECOMBINE))
  | Mapping FileHandle (WindowsOnly
    (Name, Maybe SecurityDescriptor,
    FileMappingType))
      CopyOnWrite
  | PhysicalMemory WindowsOnlyV
  | DirectMapped WindowsOnlyV

type FileHandle = Int
type CopyOnWrite = Bool

data HugePageFlags
  = MADV_NOHUGEPAGE
  | MADV_HUGEPAGE
  | LinuxOnly TransparentHugePage

data WriteWatch
  = CleanSinceReset
  | SoftDirty
  | WriteWatchDisabled WindowsOnlyV

data PageStatus
  = Reserved WindowsOnlyV
  | Committed WindowsOnlyV Committed
  | InRam LinuxOnlyV PageFrameNumber MapCount PageFrameFlags Committed
  | Swapped LinuxOnlyV SwapFileNumber SwapOffset Committed
  | NotPresent LinuxOnlyV

data Committed = C
  (WindowsOnly (WindowsMemoryProtection, NUMANode_))
  CommitStatus
  (Maybe HeapInfo)

data CommitStatus
  = NormalStatus
  | Uninitialized
  | Zeroed
  | SecurelyZeroed
  | Offered (WindowsOnly VMOfferPriority)
  | Locked
  | WithinEnclave EnclaveInfo
  | AWEAllocated WindowsOnlyV PFNBuffer

data EnclaveInfo = LinuxEnclaveSECS LinuxOnlyV |
  WindowsEnclaveInfo WindowsOnlyV
  EnclaveType
  LpEnclaveInformation
  PAGE_ENCLAVE_THREAD_CONTROL
  PAGE_ENCLAVE_UNVALIDATED

data HeapInfo = HeapInfo
  HeapHandle
  HeapSize
  (WindowsOnly HEAP_CREATE_ENABLE_EXECUTE)

type HeapHandle = Int
type HeapSize = Int

-- Linux-only:

type EffectiveNUMAPolicy = Int
type MemoryProtectionKey = Int

data ReadAdvise
  = MADV_NORMAL
  | MADV_SEQUENTIAL
  | MADV_RANDOM_READ

type MADV_DONTFORK = Bool
type MADV_WIPEONFORK = Bool
type MADV_DONTDUMP = Bool
type MADV_MERGEABLE = Bool
type MAP_GROWSDOWN = Bool
type MAP_NORESERVE = Bool
type MAP_HUGETLB = Bool
type MLOCK_ONFAULT = Bool
type MAP_SYNC = Bool
type VM_PFNMAP = Bool
type VM_DENYWRITE = Bool
type VM_IO = Bool
type VM_DONTEXPAND = Bool
type VM_ACCOUNT = Bool
type VM_MIXEDMAP = Bool
type VM_ARCH_1 = Bool
type VM_MTE = Bool
type VM_UFFD_MISSING = Bool
type VM_UFFD_WP = Bool

data VmFlags = VmFlags
  MADV_DONTFORK
  MADV_WIPEONFORK
  MADV_DONTDUMP
  MADV_MERGEABLE
  MAP_GROWSDOWN
  MAP_NORESERVE
  MAP_HUGETLB
  MLOCK_ONFAULT
  MAP_SYNC
  VM_PFNMAP
  VM_DENYWRITE
  VM_IO
  VM_DONTEXPAND
  VM_ACCOUNT
  VM_MIXEDMAP
  VM_ARCH_1
  VM_MTE
  VM_UFFD_MISSING
  VM_UFFD_WP

type PageFrameNumber = Int
type MapCount = Int
data PageFrameFlags = PFF
  HWPOISON
  IDLE
  DIRTY
  ACTIVE
  REFERENCED

type HWPOISON = Bool
type IDLE = Bool
type DIRTY = Bool
type ACTIVE = Bool
type REFERENCED = Bool

type SwapFileNumber = Int
type SwapOffset = Int

data PseudoPath = None | Stack | Vdso | Heap

type TransparentHugePage = ()

type PROT_EXEC = Bool
type PROT_READ = Bool
type PROT_WRITE = Bool

data LinuxMemoryProtection = LinuxMemoryProtection
  PROT_EXEC
  PROT_READ
  PROT_WRITE

-- Windows-only data
data FileMappingMemoryProtection
  = PAGE_EXECUTE_READ
  | PAGE_EXECUTE_READWRITE
  | PAGE_READONLY
  | PAGE_READWRITE

data AMemoryProtection
  = F FileMappingMemoryProtection
  | PAGE_EXECUTE
  | PAGE_EXECUTE_WRITECOPY
  | PAGE_WRITECOPY

data AllocatedMemoryProtection = E AMemoryProtection | PAGE_NOACCESS

type PAGE_TARGETS_INVALID = Bool
type PAGE_TARGETS_NO_UPDATE = Bool
type ValidTargetsSet = [Int]
data ControlFlowGuard = CFG
  PAGE_TARGETS_INVALID
  PAGE_TARGETS_NO_UPDATE
  ValidTargetsSet

type PAGE_GUARD = Bool
type PAGE_NOCACHE = Bool
type PAGE_WRITECOMBINE = Bool

data PageBits = PageBits PAGE_GUARD PAGE_NOCACHE PAGE_WRITECOMBINE

data AllocType = NormalAlloc | MEM_PHYSICAL | MEM_RESERVE_PLACEHOLDER | MEM_LARGE_PAGES

type Name = Maybe String
type SecurityDescriptor = ()

data AllocShared = SEC_COMMIT | SEC_RESERVE | SEC_LARGE_PAGES
type SEC_NOCACHE = Bool
type SEC_WRITECOMBINE = Bool
data FileMappingType = SEC_IMAGE | SEC_IMAGE_NO_EXECUTE | SEC_COMMIT_ SEC_NOCACHE SEC_WRITECOMBINE
  | SEC_RESERVE_ SEC_NOCACHE SEC_WRITECOMBINE
  | MEM_REPLACE_PLACEHOLDER

data WindowsMemoryProtection = W
  AllocatedMemoryProtection
  (Maybe (DwDesiredAccess, FILE_MAP_EXECUTE))

data DwDesiredAccess =  FILE_MAP_READ | FILE_MAP_WRITE | FILE_MAP_ALL_ACCESS
type FILE_MAP_EXECUTE = Bool

data VMOfferPriority
  = VMOfferPriorityVeryLow
  | VMOfferPriorityLow
  | VMOfferPriorityBelowNormal
  | VMOfferPriorityNormal

data EnclaveType = ENCLAVE_TYPE_SGX | ENCLAVE_TYPE_VBS
type LpEnclaveInformation = ()
type PAGE_ENCLAVE_THREAD_CONTROL = Bool
type PAGE_ENCLAVE_UNVALIDATED = Bool

type PFNBuffer = [Int]

type HEAP_CREATE_ENABLE_EXECUTE = Bool

data NUMANode_ = Node Int